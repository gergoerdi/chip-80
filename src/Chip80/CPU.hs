{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Chip80.CPU where

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char

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
    }

-- | `baseAddr` should be 12-bit-aligned
-- | `IY`: PC
cpu_ :: Platform -> Z80ASM
cpu_ Platform{..} = mdo

    -- Fetch next instruction
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
    timer <- labelled $ db [0]
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
        ld D [HL]
        dec HL
        ld E [HL]
        dec HL
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
        ret NZ
        jp skip

    op4 <- labelled do -- SkipNEqImm vx imm
        call loadVXtoA

        cp C
        ret Z
        jp skip

    op5 <- labelled do  -- SkipEqReg vx vy
        call loadVXtoA
        call loadVYtoC

        cp C
        ret NZ
        jp skip

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
        Z80.and 0x0f
        sla A
        ld H 0
        ld L A
        call loadVXtoA
        push IX
        call loadVYtoC
        pop IX
        ld DE funs
        add HL DE
        code [0xe9] -- jp [HL]

        mov_ <- labelled do
            ld [IX] C
            ret
        or_ <- labelled do
            Z80.or C
            ld [IX] A
            ret
        and_ <- labelled do
            Z80.and C
            ld [IX] A
            ret
        xor_ <- labelled do
            Z80.xor C
            ld [IX] A
            ret
        add_ <- labelled do
            add A C
            ld [IX] A
            ldVia A [flag] 1
            ret C
            ldVia A [flag] 0
            ret
        sub_ <- labelled do
            sub C
            ld [IX] A
            ld A 1
            ldVia A [flag] 1
            ret C
            ldVia A [flag] 0
            ret
        subNeg_ <- labelled do
            sub C
            neg
            ccf
            ld [IX] A
            ldVia A [flag] 1
            ret C
            ldVia A [flag] 0
            ret
        shiftRight_ <- labelled do
            ld A C
            rrca
            ld [IX] A
            ldVia A [flag] 1
            ret C
            ldVia A [flag] 0
            ret
        shiftLeft_ <- labelled do
            ld A C
            rlca
            ld [IX] A
            ldVia A [flag] 1
            ret C
            ldVia A [flag] 0
            ret

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
        ret Z
        jp skip

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
        ld A B
        Z80.and 0x0f
        Z80.or addressMask
        ld H A

        ld A [regs]
        add A C
        unlessFlag NC $ inc H
        ld L A

        push HL
        pop IX
        ret

    opC <- labelled do -- TODO: Randomize vx imm
        pure ()

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
        jp spritePost

    opE <- labelled do -- TODO: SkipKey vx
        -- ld A C
        -- cp 0x9e -- Skip if key is pressed
        -- jp skip
        ret

    opF <- labelled mdo
        ld A C
        cp 0x07
        jp Z getTimer
        cp 0x0a
        jp Z waitKey
        cp 0x15
        jp Z loadTimer
        cp 0x18 -- TODO: LoadSound r1
        cp 0x1e
        jp Z addPtr
        cp 0x29 -- TODO: LoadHex r1
        cp 0x33 -- TODO: StoreBCD r1
        cp 0x55
        jp Z storeRegs
        cp 0x65 -- TODO: LoadRegs r1
        ret


        getTimer <- labelled do
            call indexVXtoIX
            ldVia A [IX] [timer]
            ret

        loadTimer <- labelled do
            call loadVXtoA
            ld [timer] A
            ret

        addPtr <- labelled do
            call loadVXtoA
            ld HL [ptr]
            add A L
            ld L A
            unlessFlag NC $ inc H
            ld [ptr] HL
            ret

        waitKey <- labelled do
            loopForever $ pure ()

        storeRegs <- labelled do
            ld A B
            Z80.and 0x0f
            ld C A
            ld B 0
            inc BC
            ld HL regs
            ld DE [ptr]
            ldir
            ld [ptr] DE
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
