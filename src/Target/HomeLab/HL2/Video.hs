module Target.HomeLab.HL2.Video where

import Target.HomeLab.HL2.Defs
import Target.HomeLab.Video hiding (encodeFromPng, drawSprite)
import qualified Target.HomeLab.Video as HomeLab

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.ByteString as BS
import Codec.Picture

windowPadH :: Word16
windowPadH = (rowstride - windowWidth) `div` 2

windowPadV :: Word16
windowPadV = 3

windowStart :: Word16
windowStart = videoStart + windowPadH + windowPadV * rowstride

-- | Pre: `A` is X coordinate, `C` is Y coordinate, and `B` is sprite height
drawSprite :: Location -> Z80ASM
drawSprite = HomeLab.drawSprite blocks windowStart rowstride

blocks :: [Word8]
blocks =
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
encodeFromPng = HomeLab.encodeFromPng blocks
