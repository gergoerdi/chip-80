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

-- | `baseAddr` should be 12-bit-aligned
-- | `IY`: PC
cpu :: Location -> Z80ASM
cpu baseAddr = mdo

    -- Fetch next instruction
    ld B [IY]
    inc IY
    ld C [IY]
    inc IY

    ld A B
    Z80.and 0xf0
    replicateM_ 4 rrca
    sla A

    ld D 0
    ld E A
    ld IX ops
    add IX DE
    ld E [IX]
    inc IX
    ld D [IX]
    code [0xe9] -- jp [HL]

    ptr <- labelled $ dw [0]
    regs <- labelled $ db $ replicate 16 0
    let flag = regs + 0xf
    stack <- labelled $ dw $ replicate 24 0
    sp <- labelled $ dw [stack]

    op0 <- labelled do
        ld A C

        cp 0xe_0
        unlessFlag NZ do
            -- ClearScreen
            ret
        cp 0xe_e
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

    indexR1toIX <- labelled do
        ld A B
        Z80.and 0x0f
        ld IX regs
        ld D 0
        ld E A
        add IX DE
        ret

    loadR1toA <- labelled do
        call indexR1toIX
        ld A [IX]
        ret

    loadR2toC <- labelled do
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

    op3 <- labelled do -- SkipEqImm r1 imm
        call loadR1toA

        cp C
        ret NZ
        jp skip

    op4 <- labelled do -- SkipNEqImm r1 imm
        call loadR1toA

        cp C
        ret Z
        jp skip

    op5 <- labelled do  -- SkipEqReg r1 r2
        call loadR1toA
        call loadR2toC

        cp C
        ret NZ
        jp skip

    op6 <- labelled do -- LoadImm r1 imm
        call indexR1toIX
        ld [IX] C
        ret

    op7 <- labelled do -- AddImm r1 imm
        call loadR1toA
        add A C
        ld [IX] A
        ret

    op8 <- labelled mdo -- ALU
        ld A C
        Z80.and 0x0f
        sla A
        ld H 0
        ld L A
        call loadR1toA
        push IX
        call loadR2toC
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

    op9 <- labelled do  -- SkipNEqReg r1 r2
        call loadR1toA
        call loadR2toC

        cp C
        ret Z
        jp skip

    opA <- labelled do -- LoadPtr
        ld A B
        Z80.and 0x0f
        Z80.or addressMask
        ld H A
        ld L C
        ld [ptr] HL
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

    opC <- labelled do -- TODO: Randomize r1 imm
        pure ()

    opD <- labelled do -- TODO: DrawSprite
        pure ()

    opE <- labelled do -- TODO: SkipKey r1
        -- ld A C
        -- cp 0x9e -- Skip if key is pressed
        -- jp skip
        ret

    opF <- labelled mdo
        ld A C
        cp 0x07 -- GetTimer r1
        cp 0x0a -- WaitKey r1
        cp 0x15 -- LoadTimer r1
        cp 0x18 -- LoadSound r1
        cp 0x1e -- AddPtr r1
        jp Z addPtr
        cp 0x29 -- LoadHex r1
        cp 0x33 -- StoreBCD r1
        cp 0x55 -- StoreRegs r1
        cp 0x65 -- LoadRegs r1
        ret

        addPtr <- labelled do
            ldVia A L [ptr + 0]
            ldVia A H [ptr + 1]
            call loadR1toA
            add A L
            ldVia A [ptr + 0] L
            ret NC
            inc H
            ldVia A [ptr + 1] H
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
