{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Target.HomeLab.HL2.Video where

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
import Codec.Picture

pictureWidth :: Num a => a
pictureWidth = 64

pictureHeight :: Num a => a
pictureHeight = 32

windowWidth :: Word16
windowWidth = pictureWidth `div` 2

windowHeight :: Word16
windowHeight = pictureHeight `div` 2

windowPadH :: Word16
windowPadH = (numCols - windowWidth) `div` 2

windowPadV :: Word16
windowPadV = 3

windowStart :: Word16
windowStart = videoStart + windowPadH + windowPadV * numCols

windowEnd :: Word16
windowEnd = windowStart + (windowHeight - 1) * numCols + windowPadH + windowWidth

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
    ld HL windowStart

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
            Z80.or A
            push DE
            ld DE windowEnd
            sbc HL DE
            add HL DE
            unlessFlag NC do
                ldVia A [HL] [IY]
            pop DE
            inc HL
        pop BC
        inc IX
        ret

    isLastColumn <- labelled $ db [0]

    charmap <- labelled $ db charmapHL2
    pure ()

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

encodeFromPng :: BS.ByteString -> (Word8, Word8, [Word8])
encodeFromPng bs = (byteWidth, byteHeight, bytes)
  where
    byteWidth = fromIntegral $ pixelWidth `div` 2
    byteHeight = fromIntegral $ pixelHeight `div` 2

    bytes = concatMap encodeRow $ doubles . map doubles $ bits
      where
        encodeRow = uncurry $ zipWith encodeBlock
        encodeBlock (a, b) (c, d) = charmapHL2 !! idx
          where
            idx =
                (if a then 0b1000 else 0b0000) .|.
                (if b then 0b0100 else 0b0000) .|.
                (if c then 0b0010 else 0b0000) .|.
                (if d then 0b0001 else 0b0000)

    bits = [ [ pixelOpacity pixel /= 0
             | x <- [0..pixelWidth - 1]
             , let pixel = pixelAt img x y
             ]
           | y <- [0..pixelHeight - 1]
           ]
      where
        img = convertRGBA8 dimg

    doubles :: [a] -> [(a, a)]
    doubles (x1:x2:xs) = (x1, x2):doubles xs
    doubles [] = []

    Right dimg = decodePng bs
    pixelWidth = dynamicMap imageWidth dimg
    pixelHeight = dynamicMap imageHeight dimg