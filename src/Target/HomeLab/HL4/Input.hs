module Target.HomeLab.HL4.Input (scanKeys_) where

import Z80.Machine.HomeLab.HL34
import Z80
import Z80.Utils

import Data.Word
import Control.Monad
import Data.Bits
import Data.List (sortBy, groupBy)
import Data.Function (on)

-- | Scan the keyboard and write its state to the 16 bytes starting at `keyBuf`
--   Post: Z flag iff the run/brk key was pressed
scanKeys_ :: Location -> Location -> Z80ASM
scanKeys_ joyKeys keyBuf = mdo
    -- pageIO

    -- Scan "normal" keypad
    forM_ sortedKeymap \(keys@((addr, _):_)) -> do
        ld A [addr]
        forM_ keys \(_, (i, value)) -> do
            ld HL $ keyBuf + value
            ld [HL] 0x00
            Z80.bit i A
            unlessFlag NZ $ dec [HL]

    -- Scan joystick
    ld D 0
    ld IX joyKeys
    -- Directions
    ld A [0xe800]
    decLoopB 4 $ call scanJoystickKey
    -- Fire
    ld A [0xe801]
    call scanJoystickKey

    -- Check for CR, set Z flag accordingly
    ld A [0xe801]
    Z80.bit 1 A

    -- pageRAM
    ret

    scanJoystickKey <- labelled do
            -- Which key does this stand for?
            ld HL keyBuf
            ld E [IX]
            add HL DE
            inc IX

            rra
            ret C
            ld [HL] 0xff
            ret
    pure ()
  where
    sortedKeymap =
        groupBy ((==) `on` fst) . sortBy (compare `on` fst) $
        [ (addr, (i, value)) | (value, (addr, i)) <- zip [0..] keymap ]

keymap :: [(Location, Word8)]
keymap =
    [ (0xe80f, 1) -- 0 'X'

    , (0xe804, 1) -- 1 '1'
    , (0xe804, 2) -- 2 '2'
    , (0xe804, 3) -- 3 '3'

    , (0xe80d, 1) -- 4 'Q'
    , (0xe80f, 0) -- 5 'W'
    , (0xe809, 2) -- 6 'E'

    , (0xe808, 1) -- 7 'A'
    , (0xe80d, 3) -- 8 'S'
    , (0xe809, 1) -- 9 'D'

    , (0xe80f, 3) -- A 'Z'
    , (0xe809, 0) -- B 'C'
    , (0xe805, 0) -- C '4'
    , (0xe80d, 2) -- D 'R'
    , (0xe80a, 0) -- E 'F'
    , (0xe80e, 3) -- F 'V'
    ]
