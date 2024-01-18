module Target.HomeLab.HL4.Defs where

import Z80
import Z80.Utils
import Data.Word

videoStart :: Word16
videoStart = 0xf800

rowstride :: (Num a) => a
rowstride = 64

pageIO :: Z80ASM
pageIO = out [0xff] A

pageRAM :: Z80ASM
pageRAM = out [0x7f] A

printA :: Z80ASM
printA = call 0x0284

getKeyA :: Z80ASM
getKeyA = call 0x035b
