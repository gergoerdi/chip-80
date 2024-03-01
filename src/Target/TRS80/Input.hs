module Target.TRS80.Input (scanKeys_) where

import Z80
import Z80.Utils

import Data.Word
import Control.Monad
import Data.Bits
import Data.List (sortBy, groupBy)
import Data.Function (on)

-- | Scan the keyboard and write its state to the 16 bytes starting at `keyBuf`
--   Post: Z flag iff the run/brk key was pressed
scanKeys_ :: Location -> Z80ASM
scanKeys_ keyBuf = mdo
    -- Scan "normal" keypad
    forM_ sortedKeymap \(keys@((addr, _):_)) -> do
        ld A [addr]
        forM_ keys \(_, (i, value)) -> do
            ld HL $ keyBuf + value
            ld [HL] 0x00
            Z80.bit i A
            unlessFlag NZ $ dec [HL]

    -- TODO: scan "joystick"

    -- Check for Break, set Z flag accordingly
    ld A [0x3840]
    cpl
    Z80.bit 2 A

    ret
  where
    sortedKeymap =
        groupBy ((==) `on` fst) . sortBy (compare `on` fst) $
        [ (addr, (i, value)) | (value, (addr, i)) <- zip [0..] keymap ]

keymap :: [(Location, Word8)]
keymap =
    [ (0x3808, 0) -- 0 'X'

    , (0xe810, 1) -- 1 '1'
    , (0xe810, 2) -- 2 '2'
    , (0xe810, 3) -- 3 '3'

    , (0x3804, 1) -- 4 'Q'
    , (0x3804, 7) -- 5 'W'
    , (0x3801, 5) -- 6 'E'

    , (0x3801, 1) -- 7 'A'
    , (0x3804, 3) -- 8 'S'
    , (0x3801, 4) -- 9 'D'

    , (0x3808, 2) -- A 'Z'
    , (0x3801, 3) -- B 'C'
    , (0x3810, 4) -- C '4'
    , (0x3804, 2) -- D 'R'
    , (0x3801, 6) -- E 'F'
    , (0x3804, 6) -- F 'V'
    ]
