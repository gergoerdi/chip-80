module Target.HomeLab.HL4.Video64Small
    ( windowWidth, windowHeight, windowStart
    , drawSprite
    , encodeFromPng
    ) where

import Z80.Machine.HomeLab.HL34
import Target.HomeLab.Video2x2 hiding (encodeFromPng, drawSprite)
import qualified Target.HomeLab.Video2x2 as HomeLab

import Z80
import Data.Word
import qualified Data.ByteString as BS
import Codec.Picture

windowPadV :: Word16
windowPadV = 3

windowPadH :: Word16
windowPadH = (rowstride - windowWidth) `div` 2

windowStart :: Word16
windowStart = videoStart + windowPadV * rowstride + windowPadH

blocks :: [Word8]
blocks =
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

-- | Pre: `A` is X coordinate, `C` is Y coordinate, and `B` is sprite height
drawSprite :: Location -> Z80ASM
drawSprite = HomeLab.drawSprite blocks windowStart rowstride

encodeFromPng :: BS.ByteString -> (Word8, Word8, [Word8])
encodeFromPng = HomeLab.encodeFromPng blocks
