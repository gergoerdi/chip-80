{-# LANGUAGE RecursiveDo, BlockArguments #-}
module HL2 where

import Z80
import Z80.Utils
import Data.Word
import Data.Char

videoStart :: Word16
videoStart = 0xc001

numCols :: (Num a) => a
numCols = 40

invert :: String -> String
invert = map invert1

invert1 :: Char -> Char
invert1 = chr . (+ 0x80) . ord
