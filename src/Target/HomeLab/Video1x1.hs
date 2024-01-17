module Target.HomeLab.Video1x1 where

import CHIP80.Video

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.ByteString as BS
import Codec.Picture

windowWidth :: Word16
windowWidth = pictureWidth

windowHeight :: Word16
windowHeight = pictureHeight

-- | Pre: `A` is X coordinate, `C` is Y coordinate, and `B` is sprite height
drawSprite :: Word8 -> Word8 -> Location -> Word16 -> Location -> Z80ASM
drawSprite block0 block1 windowStart rowstride vidBuf = mdo
    let windowEnd = windowStart + (windowHeight - 1) * rowstride + windowWidth

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
    ld D 0
    ld E A
    add IX DE

    -- Calculate starting target byte into HL
    ld HL windowStart

    -- Add Y * rowstride to HL
    replicateM_ 3 $ srl C
    ld DE rowstride
    skippable \end -> loopForever do
        srl C
        unlessFlag NC $ add HL DE
        jp Z end
        sla E
        rl D

    -- Add X to HL, aligned to 8 pixels since IX points to a whole byte
    pop AF
    Z80.and 0x38
    ld D 0
    ld E A
    add HL DE

    push IY
    withLabel \loopRow -> do
        ld D 0

        call renderColumns

        ld DE $ 1 * (pictureWidth `div` 8) - 2
        add IX DE
        ld DE $ rowstride - 16
        add HL DE

        djnz loopRow

    pop IY
    ret

    renderColumns <- labelled do
        call renderColumn
        ld A [isLastColumn]
        Z80.and A
        jp Z renderColumn

        ld DE 8 -- 8 characters per 8 bits
        add HL DE
        inc IX
        ret

    renderColumn <- labelled do
        push BC
        decLoopB 8 do
            ld A [IX]
            rlca
            ld [IX] A

            ld A block0
            unlessFlag NC $ ld A block1

            -- -- Are we still in bounds?
            -- push DE
            -- Z80.or A
            -- ld DE windowEnd
            -- sbc HL DE
            -- add HL DE
            -- unlessFlag NC do
            --     ld [HL] A
            -- pop DE
            ld [HL] A
            inc HL
        pop BC
        inc IX
        ret

    isLastColumn <- labelled $ db [0]
    pure ()

encodeFromPng :: Word8 -> Word8 -> BS.ByteString -> (Word8, Word8, [Word8])
encodeFromPng block0 block1 bs = (byteWidth, byteHeight, bytes)
  where
    byteWidth = fromIntegral pixelWidth
    byteHeight = fromIntegral pixelHeight

    bytes = concatMap encodeRow bits
      where
        encodeRow = map \p -> if p then block1 else block0

    bits = [ [ pixelOpacity pixel /= 0
             | x <- [0..pixelWidth - 1]
             , let pixel = pixelAt img x y
             ]
           | y <- [0..pixelHeight - 1]
           ]
      where
        img = convertRGBA8 dimg

    Right dimg = decodePng bs
    pixelWidth = dynamicMap imageWidth dimg
    pixelHeight = dynamicMap imageHeight dimg
