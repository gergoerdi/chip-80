{-# LANGUAGE FlexibleContexts #-}
module CHIP80.CPU where

import Z80
import Z80.Utils
import LFSR

import CHIP80.Quirks
import CHIP80.Font

import Data.Word
import Control.Monad
import Data.Bits
import Data.Foldable

data Platform = Platform
    { baseAddr :: Location
    , spritePre :: Location -- ^ Called with X coordinate in `A`, Y coordinate in `C`, height in `B`
    , spritePost :: Location
    , clearScreen :: Location
    , readKeys :: Maybe Location
    }

data CPU = CPU
    { init :: Location -- ^ Pre: `HL` contains address of quirks settings
    , resetCPU :: Location
    , newFrame :: Location
    , stepCPU :: Location
    , vidBuf, keyBuf :: Location
    }

type State = Word8
stateRUN, stateWAIT_PRESS, stateWAIT_RELEASE :: State
stateRUN = 0
stateWAIT_PRESS = 1
stateWAIT_RELEASE = 2

cpu :: Platform -> Z80 CPU
cpu Platform{..} = mdo
    let vidBuf = baseAddr + 0x080 -- Leave space for the hex font
        keyBuf = vidBuf + 0x100
        ptr = keyBuf + 16
        regs = ptr + 2
        flag = regs + 0xf -- VF
        stack = flag + 1
        sp = stack + 24
        pc = sp + 2
        timer = pc + 2
        state = timer + 1
        waitForFrame = state + 1
        lastVar = waitForFrame

    -- All this needs to fit inside the first 0x200 bytes from `baseAddr`
    unless (lastVar < baseAddr + 0x200) $ error "CHIP-8 CPU state doesn't fit"

    quirks@Quirks{..} <- do
        shiftVY <- labelled $ db [1]
        resetVF <- labelled $ db [1]
        incrementPtr <- labelled $ db [1]
        videoWait <- labelled $ db [1]
        clipSprites <- labelled $ db [1]
        pure Quirks{..}

    rnd <- labelled $ dw [0xf00f]
    lfsr <- labelled lfsr10

    hex <- labelled $ db font

    init <- labelled do
        -- Zero out CHIP-8 RAM
        ld DE baseAddr
        ld A 0
        decLoopB 16 do
            ld C B
            decLoopB 256 do
                ld [DE] A
                inc DE
            ld B C

        -- Load hex font into baseAddr..+0x080
        push HL
        ld DE baseAddr
        ld HL hex
        ld BC $ 16 * 8
        ldir
        pop HL

        -- Load quirks
        forM_ [shiftVY, resetVF, incrementPtr, videoWait, clipSprites] \quirk -> do
            ldVia A [quirk] [HL]
            inc HL

        jp clearScreen

    resetCPU <- labelled do
        -- Reset CPU registers
        ld A 0

        ld HL vidBuf
        decLoopB 256 do
            ld [HL] A
            inc HL

        ld HL regs
        decLoopB 16 do
            ld [HL] A
            inc HL

        forM_ [ptr, ptr + 1, state, timer] \addr -> ld [addr] A

        let (stackLo, stackHi) = wordBytes stack
        ldVia A [sp] stackLo
        ldVia A [sp + 1] stackHi

        ldVia DE [pc] $ baseAddr + 0x200
        ret

    newFrame <- labelled do
        ldVia A [waitForFrame] 0
        ld DE [rnd]
        call lfsr
        ld [rnd] DE
        ld A [timer]
        Z80.and A
        ret Z
        dec A
        ld [timer] A
        ret

    stepCPU <- labelled mdo
        let checkQuirk quirk = do
                push HL
                push BC
                ld HL quirk
                ld C [HL]
                inc C
                dec C
                pop BC
                pop HL
            ifQuirk quirk thn els = do
                checkQuirk quirk
                skippable \end -> mdo
                    jp NZ true
                    false <- labelled do
                        els
                        jp end
                    true <- labelled do
                        thn
                    pure ()
            whenQuirk quirk body = do
                checkQuirk quirk
                unlessFlag Z body

        ld A [state]
        cp stateWAIT_PRESS
        jp Z waitPress
        cp stateWAIT_RELEASE
        jp Z waitRelease
        jp step

        prevKeyBuf <- labelled $ db $ replicate 16 0
        keyAddr <- labelled $ dw [0]

        waitPress <- labelled do
            -- Wait for a fresh keypress
            traverse_ call readKeys
            ld HL $ keyBuf + 16 -- We will be scanning downwards, because of `decLoopB`
            ld DE $ prevKeyBuf + 16
            skippable \pressed -> do
                decLoopB 16 do -- `B` will be the key found
                    -- Check for a key that wasn't pressed before, but is pressed now.
                    dec DE
                    ld A [DE]
                    cpl
                    dec HL
                    ld C [HL]
                    Z80.and C
                    jp NZ pressed

                -- Store old keyboard state
                ld DE prevKeyBuf
                ld HL keyBuf
                ld BC 16
                ldir

                ret

            dec B
            -- At this point, we have the freshly pressed key's index in `B`

            -- Write result
            ld HL [keyAddr]
            ld [HL] B

            -- Start waiting for the found key to be released
            ld C B
            ld B 0
            ld HL keyBuf
            add HL BC
            ld [keyAddr] HL

            ldVia A [state] stateWAIT_RELEASE
            ret

        waitRelease <- labelled do
            traverse_ call readKeys
            ld HL [keyAddr]
            ld A [HL]
            Z80.and A
            ret NZ

            ldVia A [state] stateRUN
            ret

        step <- label
        ld A [waitForFrame]
        Z80.and A
        ret NZ

        -- Fetch next instruction into B and C
        ld HL [pc]
        ld B [HL]
        inc HL

        -- ld A B
        -- dbgA

        ld C [HL]
        inc HL
        ld [pc] HL

        -- ld A C
        -- dbgA
        -- cr

        ld A B
        replicateM_ 4 rrca
        Z80.and 0x0f
        sla A

        ld D 0
        ld E A
        ld HL ops
        add HL DE
        ld E [HL]
        inc HL
        ld D [HL]
        push DE
        pop HL
        code [0xe9] -- jp [HL]

        op0 <- labelled do
            ld A C

            cp 0xe0
            unlessFlag NZ do -- ClearScreen
                ld HL vidBuf
                decLoopB 256 do
                    ld [HL] 0
                    inc HL
                jp clearScreen
            cp 0xee
            ret NZ

            -- Ret
            ld HL [sp]
            dec HL
            ld D [HL]
            dec HL
            ld E [HL]
            ld [sp] HL
            ld [pc] DE
            ret

        op1 <- labelled do -- Jump
            ld A B
            Z80.and 0x0f
            ld H A
            ld L C
            ld DE baseAddr
            add HL DE
            ld [pc] HL
            ret

        op2 <- labelled do -- Call
            ld HL [sp]
            ld DE [pc]
            ld [HL] E
            inc HL
            ld [HL] D
            inc HL
            ld [sp] HL
            jp op1

        indexVXtoHL <- labelled do
            ld A B
            Z80.and 0x0f
            ld HL regs
            ld D 0
            ld E A
            add HL DE
            ret

        loadVXtoA <- labelled do
            call indexVXtoHL
            ld A [HL]
            ret

        loadVYtoC <- labelled do
            push AF
            ld A C
            replicateM_ 4 rrca
            Z80.and 0x0f
            ld HL regs
            ld D 0
            ld E A
            add HL DE
            ld C [HL]
            pop AF
            ret

        skip <- labelled do
            ld HL [pc]
            ld DE 2
            add HL DE
            ld [pc] HL
            ret

        op3 <- labelled do -- SkipEqImm vx imm
            call loadVXtoA

            cp C
            jp Z skip
            ret

        op4 <- labelled do -- SkipNEqImm vx imm
            call loadVXtoA

            cp C
            jp NZ skip
            ret

        op5 <- labelled do  -- SkipEqReg vx vy
            call loadVXtoA
            call loadVYtoC

            cp C
            jp Z skip
            ret

        op6 <- labelled do -- LoadImm vx imm
            call indexVXtoHL
            ld [HL] C
            ret

        op7 <- labelled do -- AddImm vx imm
            call loadVXtoA
            add A C
            ld [HL] A
            ret

        op8 <- labelled mdo -- ALU
            -- Load funs[C] into HL
            ld A C
            rla
            Z80.and 0b0001_1110
            ld H 0
            ld L A
            ld DE funs
            add HL DE

            -- Load [HL] into DE
            ld E [HL]
            inc HL
            ld D [HL]
            push DE

            -- ALU function arguments
            call loadVXtoA
            push HL
            call loadVYtoC
            pop DE

            -- Do the jump
            pop HL
            code [0xe9] -- jp [HL]

            let setFlagFromC = do
                    ld HL flag
                    ld [HL] 1
                    ret C
                    ld [HL] 0
                    ret
                setFlagFromNC = do
                    ld HL flag
                    ld [HL] 1
                    ret NC
                    ld [HL] 0
                    ret

            mov_ <- labelled do
                ldVia A [DE] C
                ret

            or_ <- labelled do
                Z80.or C
                ld [DE] A
                whenQuirk resetVF $ ldVia A [flag] 0
                ret
            and_ <- labelled do
                Z80.and C
                ld [DE] A
                whenQuirk resetVF $ ldVia A [flag] 0
                ret
            xor_ <- labelled do
                Z80.xor C
                ld [DE] A
                whenQuirk resetVF $ ldVia A [flag] 0
                ret
            add_ <- labelled do
                add A C
                ld [DE] A
                setFlagFromC
            sub_ <- labelled do
                sub C
                ld [DE] A
                setFlagFromNC
            subFlip_ <- labelled do
                ld B A
                ld A C
                ld C B
                jp sub_

            shiftRight_ <- labelled do
                whenQuirk shiftVY $ ld A C
                srl A
                ld [DE] A
                setFlagFromC
            shiftLeft_ <- labelled do
                whenQuirk shiftVY $ ld A C
                sla A
                ld [DE] A
                setFlagFromC

            funs <- labelled $ dw
              [ mov_
              , or_
              , and_
              , xor_
              , add_
              , sub_
              , shiftRight_
              , subFlip_
              , 0x0000
              , 0x0000
              , 0x0000
              , 0x0000
              , 0x0000
              , 0x0000
              , shiftLeft_
              , 0x0000
              ]
            pure ()

        op9 <- labelled do  -- SkipNEqReg vx vy
            call loadVXtoA
            call loadVYtoC

            cp C
            jp NZ skip
            ret

        opA <- labelled do -- LoadPtr
            ld A B
            Z80.and 0x0f
            ld H A
            ld L C
            ld [ptr] HL
            ret

        opB <- labelled do -- JumpPlusV0
            ld D B

            ld A [regs]
            add A C
            unlessFlag NC $ inc D
            ld E A

            ld A D
            Z80.and 0x0f
            ld D A

            ld HL baseAddr
            add HL DE
            ld [pc] HL
            ret

        opC <- labelled do -- Randomize vx imm
            call indexVXtoHL
            ld DE [rnd]
            call lfsr
            ld [rnd] DE

            ld A E
            Z80.and C
            ld [HL] A
            ret

        opD <- labelled mdo -- DrawSprite vx vy n
            ldVia A [flag] 0 -- We'll overwrite this with 1 if we find a collision

            -- Store sprite height for later
            ld A C
            Z80.and 0x0f
            ld [spriteHeight] A

            -- Evaluate VX clamped to 0..63 into `A`
            call loadVXtoA
            Z80.and 0x3f

            -- Evaluate VY clamped to 0..31 into `C`
            call loadVYtoC
            push AF
            ld A C
            Z80.and 0x1f
            ld C A

            ldVia A B [spriteHeight]
            pop AF

            -- At this point, we have X coordinate in `A`, Y coordinate in `C`, and sprite height in `B`
            call spritePre

            -- Calculate target offset into `HL`
            push AF
            ld H 0
            replicateM_ 3 $ sla C
            replicateM_ 3 $ srl A
            add A C
            ld L A
            pop AF

            -- Calculate sub-byte bit offset
            Z80.and 0b111

            push HL
            ld HL [ptr]
            ld DE baseAddr
            add HL DE
            ex DE HL
            pop HL

            let pixelByte :: (Load A r) => r -> Z80ASM
                pixelByte r = do
                    -- Load pixel data from [vidBuf + HL] into D
                    push DE
                    push HL
                    -- `HL += vidBuf`
                    ld DE vidBuf
                    add HL DE
                    ld D [HL]

                    -- Check collision
                    ld A r
                    Z80.and D
                    unlessFlag Z $ ldVia A [flag] (1 :: Word8)

                    -- Draw pixels
                    ld A r
                    Z80.xor D
                    ld [HL] A

                    pop HL
                    pop DE

            -- `DE`: source (sprite data)
            -- `HL`: target (video buffer)
            skippable \clipVertical -> withLabel \loopRow -> do
                whenQuirk clipSprites do
                    -- Is `HL` now over line 31?
                    ld C A
                    ld A H
                    cp 1
                    jp NC clipVertical
                    ld A C
                ld H 0

                push AF
                push BC

                ld C A
                ldVia A B [DE]
                ld A C
                inc DE
                ld C 0

                skippable \end -> loopForever do
                    cp 0
                    jp Z end
                    srl B
                    rr C
                    dec A

                pixelByte B
                call drawSecondByte

                ld B 0
                ldVia A C [nextRow]
                add HL BC

                pop BC
                pop AF
                djnz loopRow
            call spritePost

            ld A [videoWait]
            Z80.and A
            ret Z
            ldVia A [waitForFrame] 1

            ret

            spriteHeight <- labelled $ db [0]
            nextRow <- labelled $ db [0]
            drawSecondByte <- labelled do
                -- Horizontal wrap-around
                ldVia A [nextRow] 7
                inc L
                ld A L
                ifQuirk clipSprites
                  (do
                          Z80.and 0b00000_111
                          ret Z

                          -- Is `HL` now wrapped over to the next line?
                          ld A L
                          Z80.and 0b111
                          ret Z)
                  (do
                          Z80.and 0b00000_111
                          unlessFlag NZ do
                              ld A L
                              sub 8
                              ld L A
                              ldVia A [nextRow] (7 + 8))
                pixelByte C
                ret
            pure ()

        opE <- labelled mdo -- SkipKey vx
            call loadVXtoA

            traverse_ call readKeys
            ld D 0
            ld E A
            ld HL keyBuf
            add HL DE

            ld A [HL]
            Z80.and A
            jp NZ pressed

            notPressed <- label
            ld A C
            cp 0x9e -- Skip if key is pressed
            jp NZ skip
            ret

            pressed <- label
            ld A C
            cp 0x9e -- Skip if key is pressed
            jp Z skip
            ret

        opF <- labelled mdo
            ld A C
            cp 0x07
            jp Z getTimer
            cp 0x0a
            jp Z waitKey
            cp 0x15
            jp Z loadTimer
            cp 0x18
            jp Z loadSound
            cp 0x1e
            jp Z addPtr
            cp 0x29
            jp Z loadHex
            cp 0x33
            jp Z storeBCD
            cp 0x55
            jp Z storeRegs
            cp 0x65
            jp Z loadRegs
            ret

            loadSound <- labelled do -- TODO: LoadSound VX
                ret

            loadHex <- labelled do -- LoadHex VX
                call loadVXtoA
                replicateM_ 3 rlca -- Multiply by 8
                Z80.and 0b0111_1000
                ld D 0
                ld E A
                ld [ptr] DE
                ret

            storeBCD <- labelled mdo -- StoreBCD VX
                call loadVXtoA

                ld HL [ptr]
                ld DE baseAddr
                add HL DE

                -- Hundreds
                cp 200
                jp NC hundred2
                cp 100
                jp NC hundred1
                ld [HL] 0

                bcd <- labelled do
                    inc HL

                    -- From https://www.msx.org/forum/development/msx-development/bcdhex-conversion-asm
                    ld C A
                    Z80.xor A
                    decLoopB 8 do
                        sla C
                        adc A A
                        daa
                tens <- labelled do
                    ld C A
                    replicateM_ 4 rrca
                    Z80.and 0x0f
                    ld [HL] A
                ones <- labelled do
                    inc HL
                    ld A C
                    Z80.and 0x0f
                    ld [HL] A
                ret

                hundred2 <- labelled do
                    sub 200
                    ld [HL] 2
                    jp bcd
                hundred1 <- labelled do
                    sub 100
                    ld [HL] 1
                    jp bcd
                pure ()

            getTimer <- labelled do -- GetTimer VX
                call indexVXtoHL
                ldVia A [HL] [timer]
                ret

            loadTimer <- labelled do -- LoadTimer VX
                call loadVXtoA
                ld [timer] A
                ret

            addPtr <- labelled do -- AddPtr VX
                call loadVXtoA
                ld HL [ptr]

                -- Add A to low byte
                add A L
                unlessFlag NC $ inc H
                ld L A

                -- Renormalize high byte
                ld A H
                Z80.and 0x0f
                ld H A

                ld [ptr] HL
                ret

            waitKey <- labelled do -- WaitKey VX
                call indexVXtoHL
                ld [keyAddr] HL

                -- Initialize `prevKeyBuf`
                traverse_ call readKeys
                ld HL keyBuf
                ld DE prevKeyBuf
                ld BC 16
                ldir

                -- Go to state waitPress
                ldVia A [state] stateWAIT_PRESS
                ret

            storeRegs <- labelled do -- StoreRegs VX
                -- Set BC to number of registers to store
                ld A B
                Z80.and 0x0f
                ld C A
                ld B 0
                inc BC

                -- Set DE to memory address
                ld HL [ptr]
                ld DE baseAddr
                add HL DE
                ex DE HL

                ld HL regs

                push BC -- We'll need this to compute new value of pointer register
                ldir
                pop BC
                whenQuirk incrementPtr do
                    ld HL [ptr]
                    add HL BC
                    ld [ptr] HL
                ret

            loadRegs <- labelled do -- LoadRegs VX
                -- Set BC to number of registers to store
                ld A B
                Z80.and 0x0f
                ld C A
                ld B 0
                inc BC

                -- Set HL to memory address
                ld HL [ptr]
                ld DE baseAddr
                add HL DE

                ld DE regs

                push BC
                ldir
                pop BC
                whenQuirk incrementPtr do
                    ld HL [ptr]
                    add HL BC
                    ld [ptr] HL
                ret
            pure ()

        ops <- labelled $ dw
          [ op0
          , op1
          , op2
          , op3
          , op4
          , op5
          , op6
          , op7
          , op8
          , op9
          , opA
          , opB
          , opC
          , opD
          , opE
          , opF
          ]
        pure ()

    pure CPU{..}
