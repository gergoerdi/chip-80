module Target.HomeLab.HL4.Video64
    ( windowWidth, windowHeight, windowStart
    , drawSprite
    , encodeFromPng
    ) where

import Z80.Machine.HomeLab.HL34
import Target.HomeLab.Video1x1 hiding (encodeFromPng, drawSprite)
import qualified Target.HomeLab.Video1x1 as HomeLab

import Z80
import Data.Word
import qualified Data.ByteString as BS
import Codec.Picture

windowPadV :: Word16
windowPadV = 0

windowStart :: Word16
windowStart = videoStart + windowPadV * rowstride

block0, block1 :: Word8
block0 = 0x00
block1 = 0xff

-- | Pre: `A` is X coordinate, `C` is Y coordinate, and `B` is sprite height
drawSprite :: Location -> Z80ASM
drawSprite = HomeLab.drawSprite block0 block1 windowStart rowstride

encodeFromPng :: BS.ByteString -> (Word8, Word8, [Word8])
encodeFromPng = HomeLab.encodeFromPng block0 block1
