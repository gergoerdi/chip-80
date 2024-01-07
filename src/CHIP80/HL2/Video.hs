{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module CHIP80.HL2.Video where

import HL2
import CHIP80.CPU
import CHIP80.Font

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

-- | Pre: `A` is X coordinate, `C` is Y coordinate, and `B` is sprite height
drawSprite :: Location -> Z80ASM
drawSprite vidBuf = mdo
    -- Calculate starting source byte into IX
    ld IX vidBuf
    push AF
    replicateM_ 3 rrca
    Z80.and 0x07

    ld HL isLastColumn
    ld [HL] 0
    cp 7
    unlessFlag NZ $ ld [HL] 0xff

    replicateM_ 3 $ sla C
    Z80.or C
    -- Normalize to even rows
    Z80.and 0b1111_0111
    ld D 0
    ld E A
    add IX DE

    -- Calculate starting target byte into HL
    ld HL $ videoStart + 4 + 40 * 3

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
    srl B
    inc B
    withLabel \loopRow -> do
        ld D 0

        call renderColumns

        ld DE $ fromIntegral pictureWidth `div` 8 + (fromIntegral pictureWidth `div` 8 - 2)
        add IX DE
        ld DE $ numCols - 8
        add HL DE

        djnz loopRow

    pop IY
    ret

    renderColumns <- labelled do
        call renderColumn
        ld A [isLastColumn]
        Z80.and A
        jp Z renderColumn

        ld DE 4
        add HL DE
        inc IX
        ret

    renderColumn <- labelled do
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

            -- Are we still in bounds?
            ld A H
            cp 0xc3
            unlessFlag NC do
                ldVia A [HL] [IY]
            inc HL
        pop BC
        inc IX
        ret

    isLastColumn <- labelled $ db [0]

    charmap <- labelled $ db
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
    pure ()
