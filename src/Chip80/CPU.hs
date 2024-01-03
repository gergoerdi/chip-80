{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Chip80.CPU where

import Z80
import Z80.Utils
import LFSR

import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char
import Data.Default

data Quirks = Quirks
    { shiftVY, resetVF, incrementPtr, videoWait, clipSprites :: Bool
    }
    deriving (Show)

instance Default Quirks where
    def = Quirks
        { shiftVY = True
        , resetVF = True
        , incrementPtr = True
        , videoWait = True
        , clipSprites = True
        }

dbgA :: Z80ASM
dbgA = do
    call 0x01a5

cr = do
    ld A 0x0d
    rst 0x28

space = do
    ld A 0x20
    rst 0x28

data Platform = Platform
    { baseAddr, vidAddr :: Location
    , spritePre :: Location
    , spritePost :: Location
    , clearScreen :: Location
    , scanKeys :: Location
    , keyBuf :: Location
    , timer :: Location
    }

newFrame_ :: Platform -> Z80ASM
newFrame_ Platform{..} = do
    ld A [timer]
    dec A
    ret C
    ld [timer] A
    ret

-- | `baseAddr` should be 12-bit-aligned
-- | `IY`: PC
cpu_ :: Quirks -> Platform -> Z80ASM
cpu_ Quirks{..} Platform{..} = mdo
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
        call scanKeys

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
        call scanKeys
        ld IX [keyAddr]
        ld A [IX]
        Z80.and A
        ret NZ

        ldVia A [state] 0
        ret

    step <- label
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
        Z80.or addressMask
        ld H A
        ld L C
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
            when resetVF $ ldVia A [flag] 0
            ret
        and_ <- labelled do
            Z80.and C
            ld [IX] A
            when resetVF $ ldVia A [flag] 0
            ret
        xor_ <- labelled do
            Z80.xor C
            ld [IX] A
            when resetVF $ ldVia A [flag] 0
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
            if shiftVY then do
                srl C
                ld [IX] C
              else do
                srl A
                ld [IX] A
            setFlagFromC
        shiftLeft_ <- labelled do
            if shiftVY then do
                sla C
                ld [IX] C
              else do
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
        -- space
        -- space

        ld A B
        Z80.and 0x0f
        Z80.or addressMask
        ld H A
        -- dbgA
        ld L C
        ld [ptr] HL
        -- ld A C
        -- dbgA
        -- cr
        ret

    opB <- labelled do -- JumpPlusV0
        ld H B

        ld A [regs]
        add A C
        unlessFlag NC $ inc H
        ld L A

        ld A H
        Z80.and 0x0f
        Z80.or addressMask
        ld H A

        push HL
        pop IY
        ret

    lfsr <- labelled lfsr10
    rnd <- labelled $ dw [1]

    opC <- labelled do -- Randomize vx imm
        call indexVXtoIX
        ld DE [rnd]
        call lfsr
        ld [rnd] DE

        ld A E
        Z80.and C
        ld [IX] A
        ret

    opD <- labelled do -- DrawSprite vx vy n
        ld A C
        Z80.and 0x0f
        ld H A
        call loadVXtoA
        call loadVYtoC
        ld B H

        -- At this point, we have X coordinate in `A`, Y coordinate in `C`, and sprite height in `B`
        call spritePre

        -- Calculate target offset
        push AF
        ld D 0
        replicateM_ 3 $ sla C
        replicateM_ 3 $ srl A
        add A C
        ld E A
        ld HL vidAddr
        add HL DE

        -- Calculate sub-byte bit offset
        pop AF
        Z80.and 0b111

        ld IX [ptr]

        -- `IX`: source (sprite data)
        -- `HL`: target (video buffer)
        withLabel \loop -> do
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

            ld C [HL]
            ld A D
            Z80.xor C
            ld [HL] A

            inc HL
            ld C [HL]
            ld A E
            Z80.xor C
            ld [HL] A

            ld DE 7
            add HL DE

            pop AF
            djnz loop
        -- cr
        call spritePost
        when videoWait halt
        ret

    opE <- labelled mdo -- SkipKey vx
        call loadVXtoA

        ld D 0
        ld E A
        ld IX keyBuf
        add IX DE

        call scanKeys
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
            replicateM_ 3 do
                rlca
            Z80.and 0b0111_1000
            ld DE baseAddr
            ld E A
            ld [ptr] DE
            ret

        storeBCD <- labelled mdo -- StoreBCD VX
            call loadVXtoA
            ld HL [ptr]

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
            add A L
            ld L A
            unlessFlag NC $ inc H
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
            ld A B
            Z80.and 0x0f
            ld C A
            ld B 0
            inc BC
            ld DE [ptr]
            ld A D
            Z80.and 0x0f
            Z80.or addressMask
            ld D A
            ld HL regs
            ldir
            when incrementPtr $ ld [ptr] DE
            ret

        loadRegs <- labelled do -- LoadRegs VX
            ld A B
            Z80.and 0x0f
            ld C A
            ld B 0
            inc BC
            ld DE regs
            ld HL [ptr]
            ld A H
            Z80.and 0x0f
            Z80.or addressMask
            ld H A
            ldir
            when incrementPtr $ ld [ptr] DE
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
  where
    addressMask :: Word8
    addressMask = fromIntegral $ (baseAddr .&. 0xf000) `shiftR` 8
