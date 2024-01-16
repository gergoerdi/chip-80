module Target.HomeLab.HL4.Defs where

import Z80
import Z80.Utils
import Data.Word

videoStart :: Word16
videoStart = 0xf800

numCols :: (Num a) => a
numCols = 32

pageVideo :: Z80ASM
pageVideo = out [0xff] A

pageRAM :: Z80ASM
pageRAM = out [0x7f] A
