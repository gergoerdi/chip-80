{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards, NamedFieldPuns #-}
module CHIP80.HL2.Input (scanKeys_) where

import Z80
import Z80.Utils
import HL2

import Data.Word
import Control.Monad
import Data.Bits
import Data.List (sortBy, groupBy)
import Data.Function (on)

-- | Scan the keyboard and write its state to the 16 bytes starting at `keyBuf`
scanKeys_ :: Location -> Z80ASM
scanKeys_ keyBuf = do
    forM_ sortedKeymap \(keys@((addr, _):_)) -> do
        ld A [addr]
        forM_ keys \(_, (i, value)) -> do
            ld HL $ keyBuf + value
            ld [HL] 0x00
            Z80.bit i A
            unlessFlag NZ $ dec [HL]
    ret
  where
    sortedKeymap =
        groupBy ((==) `on` fst) . sortBy (compare `on` fst) $
        [ (addr, (i, value)) | (value, (addr, i)) <- zip [0..] keymap ]

keymap :: [(Location, Word8)]
keymap =
    [ (0x3a7f, 0) -- 0 'X'

    , (0x3afb, 1) -- 1 '1'
    , (0x3afb, 2) -- 2 '2'
    , (0x3afb, 3) -- 3 '3'

    , (0x3abf, 1) -- 4 'Q'
    , (0x3abf, 7) -- 5 'W'
    , (0x3aef, 5) -- 6 'E'

    , (0x3aef, 1) -- 7 'A'
    , (0x3abf, 3) -- 8 'S'
    , (0x3aef, 4) -- 9 'D'

    , (0x3a7f, 2) -- A 'Z'
    , (0x3aef, 3) -- B 'C'
    , (0x3afb, 4) -- C '4'
    , (0x3abf, 2) -- D 'R'
    , (0x3aef, 6) -- E 'F'
    , (0x3abf, 6) -- F 'V'
    ]
