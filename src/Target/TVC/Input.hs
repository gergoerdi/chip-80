module Target.TVC.Input (scanKeys_) where

import Target.TVC.Defs
import Z80
import Z80.Utils

import Data.Word
import Control.Monad
import Data.Bits
import Data.List (sortBy, groupBy)
import Data.Function (on)

-- | Scan the keyboard and write its state to the 16 bytes starting at `HL`
--   Post: Z flag iff the run/brk key was pressed
scanKeys_ :: Z80ASM
scanKeys_ = do
    ld A [0x0b11]
    Z80.and 0xf0
    ld D A

    forM_ sortedKeymap \(keys@((addr, _):_)) -> do
        ld A D
        ld C addr
        Z80.or C

        out [0x03] A
        in_ A [0x58]

        forM_ keys \(_, (i, value)) -> do
            ld DE (fromIntegral value)
            push HL
            add HL DE
            ld [HL] 0x00
            Z80.bit i A
            unlessFlag NZ $ dec [HL]
            pop HL

    -- TODO: check DEL for reset, set Z if pressed
    ret
  where
    sortedKeymap =
        groupBy ((==) `on` fst) . sortBy (compare `on` fst) $
        [ (addr, (i, value))
        | (value, idx) <- zip [0..] keymap
        , let (addr, i) = idx `divMod` 8
        ]

keymap =
    [ 0x32 -- 0 'X'

    , 0x06 -- 1 '1'
    , 0x02 -- 2 '2'
    , 0x01 -- 3 '3'

    , 0x16 -- 4 'Q'
    , 0x12 -- 5 'W'
    , 0x11 -- 6 'E'

    , 0x26 -- 7 'A'
    , 0x22 -- 8 'S'
    , 0x21 -- 9 'D'

    , 0x36 -- A 'Y'
    , 0x31 -- B 'C'
    , 0x07 -- C '4'
    , 0x17 -- D 'R'
    , 0x27 -- E 'F'
    , 0x37 -- F 'V'
    ]
