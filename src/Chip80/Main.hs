{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Chip80.Main (game) where

import HL2
import Chip80.CPU

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.ByteString as BS

pictureWidth :: Word8
pictureWidth = 64

pictureHeight :: Word8
pictureHeight = 32

game :: BS.ByteString -> Z80ASM
game image = mdo
    -- Load program into CHIP-8 RAM
    ld HL prog
    ld DE 0x7200
    ld BC (0x1000 - 0x200)
    ldir

    ld IY 0x7200
    decLoopB 35 do
        push BC
        call cpu
        pop BC

    loopForever $ pure ()

    vidBuf <- labelled $ db $ replicate (8 * 32) 0
    cpu <- labelled $ cpu_ Platform{ baseAddr = 0x7000, vidAddr = vidBuf, spritePre, spritePost, clearScreen }
    prog <- labelled $ db image

    clearScreen <- labelled do
        ld HL videoStart
        withLabel \loop -> do
            ld [HL] 0x20
            inc HL
            ld A H
            cp 0xc4
            jp NZ loop
        ret

    spritePre <- labelled do
        ld [spriteX] A
        push AF
        ldVia A [spriteY] C
        ldVia A [spriteH] B
        pop AF
        ret

    spritePost <- label
    ld A 0x00
    spriteX <- subtract 1 <$> label
    ld C 0x00
    spriteY <- subtract 1 <$> label
    ld B 0x00
    spriteH <- subtract 1 <$> label
    do
        -- At this point, we have X coordinate in `A`, Y coordinate in `C`, and sprite height in `B`

        -- Calculate starting source byte into IX
        ld IX vidBuf
        push AF
        replicateM_ 3 rrca
        Z80.and 0x07
        replicateM_ 3 $ sla C
        Z80.or C
        -- Normalize to even rows
        Z80.and 0b1111_0111
        ld D 0
        ld E A
        add IX DE

        -- Calculate starting target byte into HL
        ld HL videoStart

        -- Add Y/2 * 40 to HL
        replicateM_ 4 $ srl C
        ld DE 40
        skippable \end -> loopForever do
            srl C
            unlessFlag NC $ add HL DE
            jp Z end
            sla E
            rl D

        -- Add X / 2 to HL, aligned to 8 pixels since IX points to a whole byte
        pop AF
        replicateM_ 1 rrca
        Z80.and 0x1c
        ld D 0
        ld E A
        add HL DE

        push IY
        inc B
        srl B
        withLabel \loopRow -> do
            ld D 0

            push BC
            decLoopB 2 do
                push BC
                decLoopB 4 do
                    -- Calculate into E the next 2x2 bit pattern
                    ld E 0

                    ld A [IX + 0]
                    rlca
                    rl E
                    rlca
                    rl E
                    ld [IX + 0] A

                    ld A [IX + fromIntegral (pictureWidth `div` 8)]
                    rlca
                    rl E
                    rlca
                    rl E
                    ld [IX + fromIntegral (pictureWidth `div` 8)] A

                    -- Convert bit pattern into character
                    ld IY charmap
                    add IY DE
                    ldVia A [HL] [IY]
                    inc HL
                pop BC
                inc IX
            pop BC

            ld DE $ fromIntegral pictureWidth `div` 8 + (fromIntegral pictureWidth `div` 8 - 2)
            add IX DE
            ld DE $ numCols - 8
            add HL DE

            djnz loopRow

        pop IY
        ret


    charmap <- labelled $ db charmapHL2

    pure ()

charmapHL4 :: [Word8]
charmapHL4 =
    [ 0x00 -- 00_00
    , 0x1d -- 00_01
    , 0x1e -- 00_10
    , 0x12 -- 00_11
    , 0x1c -- 01_00
    , 0xea -- 01_01
    , 0x19 -- 01_10
    , 0x18 -- 01_11
    , 0x1b -- 10_00
    , 0x1a -- 10_01
    , 0xd5 -- 10_10
    , 0x15 -- 10_11
    , 0x1f -- 11_00
    , 0x17 -- 11_01
    , 0x16 -- 11_10
    , 0xff -- 11_11
    ]

charmapHL2 :: [Word8]
charmapHL2 =
    [ 0x20 -- 00_00
    , 0xf2 -- 00_01
    , 0xf1 -- 00_10
    , 0x9c -- 00_11
    , 0xf0 -- 01_00
    , 0xfa -- 01_01
    , 0xf4 -- 01_10
    , 0xf8 -- 01_11
    , 0xef -- 10_00
    , 0xf3 -- 10_01
    , 0xe4 -- 10_10
    , 0xf7 -- 10_11
    , 0xf9 -- 11_00
    , 0xf6 -- 11_01
    , 0xf5 -- 11_10
    , 0xe0 -- 11_11
    ]
