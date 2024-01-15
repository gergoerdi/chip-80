module Target.TVC.Video where

import Target.TVC.Defs
-- import Target.HomeLab.HL2.Machine
-- import Target.HomeLab.HL2.Video (encodeFromPng)

import Z80
import Z80.Utils
import Data.Word
import Control.Monad
import qualified Data.ByteString as BS
import Codec.Picture

setVideoMode :: Z80ASM
setVideoMode = do
    ld A [0x0b13]
    Z80.and 0b1111_1100
    Z80.or  0b0000_0010 -- Graphics mode 16
    out [0x06] A

clearFullScreen :: Z80ASM
clearFullScreen = do
    pageVideo
    ld HL videoStart
    ld A 0
    decLoopB 240 do
        ld C B
        decLoopB 64 do
            ld [HL] A
            inc HL
        ld B C
    pageRAM

clearPicture :: Word8 -> Location -> Z80ASM
clearPicture scaleY frameBuf = do
    ld HL frameBuf
    nTimes scaleY do
        decLoopB 8 do
            ld C B
            decLoopB 256 do
                ld [HL] 0
                inc HL
            ld B C
  where
    nTimes 0 act = pure ()
    nTimes 1 act = act
    nTimes n act = decLoopB n do
        push BC
        act
        pop BC


blitPicture :: Int -> Location -> Z80ASM
blitPicture scaleY frameBuf = do
    pageVideo
    ld DE videoStart
    ld HL frameBuf
    decLoopB 32 do
        ld A B
        replicateM_ (scaleY - 1) do
            push HL
            ld BC 64
            ldir
            pop HL
        ld BC 64
        ldir
        ld B A
    pageRAM

-- | Pre: `A` is sprite X coordinate, `C` is sprite Y coordinate, `B` is sprite height
drawPicture :: Int -> Location -> Location -> Z80ASM
drawPicture scaleY vidBuf frameBuf = do
    -- Calculate 8 * C into HL
    push BC
    ld HL vidBuf
    ld DE 8
    skippable \end -> loopForever do
        srl C
        unlessFlag NC $ add HL DE
        jp Z end
        sla E
        rl D
    pop BC

    -- Calculate 64 * C into IX
    ld IX frameBuf
    ld DE $ 64 * fromIntegral scaleY
    skippable \end -> loopForever do
        srl C
        unlessFlag NC $ add IX DE
        jp Z end
        sla E
        rl D

    withLabel \drawRow -> do
        push BC
        decLoopB 8 do
            ld A [HL]
            inc HL

            ld C B
            decLoopB 8 do
                rlca
                ld D 0x00
                unlessFlag NC $ ld D 0xff
                forM_ [0..scaleY] \i -> do
                    ld [IX + fromIntegral i * 64] D
                inc IX
            ld B C

        when (scaleY > 1) do
            ld DE (64 * (fromIntegral scaleY - 1))
            add IX DE
        pop BC
        djnz drawRow

encodeFromPng :: BS.ByteString -> (Word8, Word8, [Word8])
encodeFromPng bs = (byteWidth, byteHeight, bytes)
  where
    byteWidth = fromIntegral pixelWidth
    byteHeight = fromIntegral pixelHeight

    bytes = concatMap encodeRow bits
      where
        encodeRow = map encodeBit
        encodeBit b = if b then 0xff else 0x00

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
