{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module CHIP80.CPU where

import Z80
import Z80.Utils

import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char
import Data.Default

data Quirks a = Quirks
    { shiftVY, resetVF, incrementPtr, videoWait, clipSprites :: a
    }
    deriving (Show)

instance Default (Quirks Bool) where
    def = Quirks
        { shiftVY = True
        , resetVF = True
        , incrementPtr = True
        , videoWait = True
        , clipSprites = True
        }

data Platform = Platform
    { baseAddr, vidAddr :: Location
    , spritePre :: Location
    , spritePost :: Location
    , clearScreen :: Location
    , keyBuf :: Location
    , timer :: Location
    , waitForFrame :: Location
    , lfsrDE :: Location
    , rnd :: Location
    }

newFrame_ :: Platform -> Z80ASM
newFrame_ Platform{..} = do
    ldVia A [waitForFrame] 0
    ld DE [rnd]
    call lfsrDE
    ld [rnd] DE
    ld A [timer]
    dec A
    ret C
    ld [timer] A
    ret

-- | `IY`: PC
cpu_ :: Quirks Location -> Platform -> Z80ASM
cpu_ Quirks{..} Platform{..} = mdo
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
    cp 1
    jp Z waitPress
    cp 2
    jp Z waitRelease
    jp step

    state <- labelled $ db [0]
    prevKeyBuf <- labelled $ db $ replicate 16 0
    keyAddr <- labelled $ dw [0]

    waitPress <- labelled do
        -- Wait for a fresh keypress
        ld HL keyBuf
        ld DE prevKeyBuf
        ld B 0 -- This will be the key found
        skippable \pressed -> do
            withLabel \loop -> do
                -- Check for a key that wasn't pressed before, but is pressed now.
                ld A [DE]
                inc DE
                cpl
                ld C [HL]
                inc HL
                Z80.and C
                jp NZ pressed

                inc B
                ld A B
                cp 16
                jp NZ loop

            -- Store old keyboard state
            ld DE prevKeyBuf
            ld HL keyBuf
            ld BC 16
            ldir

            ret

        -- Write result
        ld IX [keyAddr]
        ld [IX] B

        -- Wait for the found key to be released
        ld C B
        ld B 0
        ld IX keyBuf
        add IX BC
        ld [keyAddr] IX

        ldVia A [state] 2
        ret

    waitRelease <- labelled do
        ld IX [keyAddr]
        ld A [IX]
        Z80.and A
        ret NZ

        ldVia A [state] 0
        ret

    step <- label
    ld A [waitForFrame]
    Z80.and A
    ret NZ

    -- Fetch next instruction into B and C
    ld B [IY]
    inc IY

    -- ld A B
    -- dbgA

    ld C [IY]
    inc IY

    -- ld A C
    -- dbgA
    -- cr

    ld A B
    replicateM_ 4 rrca
    Z80.and 0x0f
    sla A

    ld D 0
    ld E A
    ld IX ops
    add IX DE
    ld L [IX]
    inc IX
    ld H [IX]
    code [0xe9] -- jp [HL]

    ptr <- labelled $ dw [0]
    regs <- labelled $ db $ replicate 16 0
    let flag = regs + 0xf
    stack <- labelled $ dw $ replicate 24 0
    sp <- labelled $ dw [stack]

    op0 <- labelled do
        ld A C

        cp 0xe0
        unlessFlag NZ do -- ClearScreen
            ld IX vidAddr
            decLoopB 256 do
                ld [IX] 0
                inc IX
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
        push DE
        pop IY
        ret

    op1 <- labelled do -- Jump
        ld A B
        Z80.and 0x0f
        ld H A
        ld L C
        ld DE baseAddr
        add HL DE
        push HL
        pop IY
        ret

    op2 <- labelled do -- Call
        ld HL [sp]
        push IY
        pop DE
        ld [HL] E
        inc HL
        ld [HL] D
        inc HL
        ld [sp] HL
        jp op1

    indexVXtoIX <- labelled do
        ld A B
        Z80.and 0x0f
        ld IX regs
        ld D 0
        ld E A
        add IX DE
        ret

    loadVXtoA <- labelled do
        call indexVXtoIX
        ld A [IX]
        ret

    loadVYtoC <- labelled do
        push AF
        ld A C
        replicateM_ 4 rrca
        Z80.and 0x0f
        ld IX regs
        ld D 0
        ld E A
        add IX DE
        ld C [IX]
        pop AF
        ret

    skip <- labelled do
        ld DE 2
        add IY DE
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
        call indexVXtoIX
        ld [IX] C
        ret

    op7 <- labelled do -- AddImm vx imm
        call loadVXtoA
        add A C
        ld [IX] A
        ret

    op8 <- labelled mdo -- ALU
        ld A C
        rla
        Z80.and 0b0001_1110
        ld H 0
        ld L A
        call loadVXtoA
        push IX
        call loadVYtoC
        pop IX
        ld DE funs
        add HL DE

        ld E [HL]
        inc HL
        ld D [HL]
        ex DE HL
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
            ld [IX] C
            ret

        or_ <- labelled do
            Z80.or C
            ld [IX] A
            whenQuirk resetVF $ ldVia A [flag] 0
            ret
        and_ <- labelled do
            Z80.and C
            ld [IX] A
            whenQuirk resetVF $ ldVia A [flag] 0
            ret
        xor_ <- labelled do
            Z80.xor C
            ld [IX] A
            whenQuirk resetVF $ ldVia A [flag] 0
            ret
        add_ <- labelled do
            add A C
            ld [IX] A
            setFlagFromC
        sub_ <- labelled do
            sub C
            ld [IX] A
            setFlagFromNC
        subNeg_ <- labelled do
            ld D A
            ld A C
            sub D
            ld [IX] A
            setFlagFromNC

        shiftRight_ <- labelled do
            whenQuirk shiftVY $ ld A C
            srl A
            ld [IX] A
            setFlagFromC
        shiftLeft_ <- labelled do
            whenQuirk shiftVY $ ld A C
            sla A
            ld [IX] A
            setFlagFromC

        funs <- labelled $ dw
          [ mov_
          , or_
          , and_
          , xor_
          , add_
          , sub_
          , shiftRight_
          , subNeg_
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

        ld IY baseAddr
        add IY DE
        ret

    opC <- labelled do -- Randomize vx imm
        call indexVXtoIX
        ld DE [rnd]
        call lfsrDE
        ld [rnd] DE

        ld A E
        Z80.and C
        ld [IX] A
        ret

    opD <- labelled mdo -- DrawSprite vx vy n
        ldVia A [flag] 0 -- We'll overwrite this with 1 if we find a collision

        -- Evaluate VX clamped to 0..63 into `A`
        ld A C
        Z80.and 0x0f
        ld H A
        call loadVXtoA
        Z80.and 0x3f

        -- Evaluate VY clamped to 0..31 into `C`
        call loadVYtoC
        push AF
        ld A C
        Z80.and 0x1f
        ld C A
        pop AF

        ld B H

        -- At this point, we have X coordinate in `A`, Y coordinate in `C`, and sprite height in `B`
        call spritePre

        -- Calculate target offset
        push AF
        ld H 0
        replicateM_ 3 $ sla C
        replicateM_ 3 $ srl A
        add A C
        ld L A

        -- Calculate sub-byte bit offset
        pop AF
        Z80.and 0b111

        ld IX [ptr]
        ld DE baseAddr
        add IX DE

        -- `IX`: source (sprite data)
        -- `HL`: target (video buffer)
        skippable \clipVertical -> withLabel \loopRow -> do
            whenQuirk clipSprites do
                -- Is `HL` now over line 31?
                ld E A
                ld A H
                cp 1
                jp NC clipVertical
                ld A E
            ld H 0

            push AF

            ld D [IX]

            ld E 0
            inc IX
            skippable \end -> loopForever do
                cp 0
                jp Z end
                srl D
                rr E
                dec A

            push HL
            push DE
            ld DE vidAddr
            add HL DE
            pop DE

            -- Check collision
            ld C [HL]
            ld A D
            Z80.and C
            unlessFlag Z $ ldVia A [flag] 1

            -- Draw pixels
            ld A D
            Z80.xor C
            ld [HL] A

            pop HL

            call drawSecondByte

            ld D 0
            ldVia A E [nextRow]
            add HL DE

            pop AF
            djnz loopRow
        call spritePost

        ld A [videoWait]
        Z80.and A
        ret Z
        ldVia A [waitForFrame] 1

        ret

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

            push HL
            push DE
            ld DE vidAddr
            add HL DE
            pop DE

            -- Check collision
            ld C [HL]
            ld A E
            Z80.and C
            unlessFlag Z $ ldVia A [flag] 1

            -- Draw pixels
            ld A E
            Z80.xor C
            ld [HL] A

            pop HL
            ret
        pure ()

    opE <- labelled mdo -- SkipKey vx
        call loadVXtoA

        ld D 0
        ld E A
        ld IX keyBuf
        add IX DE

        ld A [IX]
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
            call indexVXtoIX
            ldVia A [IX] [timer]
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
            call indexVXtoIX
            ld [keyAddr] IX

            -- Initialize `prevKeyBuf`
            ld DE prevKeyBuf
            ld A 0xff
            decLoopB 16 do
                ld [DE] A
                inc DE

            -- Go to state waitPress
            ldVia A [state] 1
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
