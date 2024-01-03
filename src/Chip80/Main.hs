{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Chip80.Main (game) where

import HL2
import Chip80.CPU

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.ByteString as BS
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Default

pictureWidth :: Word8
pictureWidth = 64

pictureHeight :: Word8
pictureHeight = 32

game :: BS.ByteString -> Z80ASM
game image = mdo
    let baseAddr = 0x7000
        kbdBuf = baseAddr + 0x080
        vidBuf = baseAddr + 0x100
    ld SP $ baseAddr - 1

    ld DE baseAddr
    ld A 0
    decLoopB 16 do
        push BC
        decLoopB 256 do
            ld [DE] A
            inc DE
        pop BC

    -- Load hex font
    ld DE baseAddr
    ld HL hex
    ld BC $ 16 * 8
    ldir

    -- Load program into CHIP-8 RAM
    ld HL prog
    ld DE $ baseAddr + 0x200
    ld BC $ fromIntegral $ BS.length image -- (0x1000 - 0x200)
    ldir

    ld IY $ baseAddr + 0x200
    loopForever do
        call cpu
        ld HL lastFrame
        ld A [0x403f]
        cp [HL]
        unlessFlag Z do
            ld [lastFrame] A
            call newFrame

    timer <- labelled $ db [0]
    lastFrame <- labelled $ db [0]

    let platform = Platform{ vidAddr = vidBuf, .. }
    cpu <- labelled $ cpu_ def platform
    newFrame <- labelled $ newFrame_ platform
    prog <- labelled $ db image

    clearScreen <- labelled do
        ld HL videoStart
        withLabel \loop -> do
            ld [HL] 0x20
            inc HL
            ld A H
            cp 0xc4
            jp NZ loop
        ret

    spritePre <- labelled do
        ld [spriteX] A
        push AF
        ldVia A [spriteY] C
        ldVia A [spriteH] B
        pop AF
        ret

    isLastColumn <- labelled $ db [0]

    spritePost <- label
    ld A 0x00
    spriteX <- subtract 1 <$> label
    ld C 0x00
    spriteY <- subtract 1 <$> label
    ld B 0x00
    spriteH <- subtract 1 <$> label
    mdo
        -- At this point, we have X coordinate in `A`, Y coordinate in `C`, and sprite height in `B`

        -- Calculate starting source byte into IX
        ld IX vidBuf
        push AF
        replicateM_ 3 rrca
        Z80.and 0x07

        ld HL isLastColumn
        ld [HL] 0
        cp 7
        unlessFlag NZ $ ld [HL] 0xff

        replicateM_ 3 $ sla C
        Z80.or C
        -- Normalize to even rows
        Z80.and 0b1111_0111
        ld D 0
        ld E A
        add IX DE

        -- Calculate starting target byte into HL
        ld HL videoStart

        -- Add Y/2 * 40 to HL
        replicateM_ 4 $ srl C
        ld DE 40
        skippable \end -> loopForever do
            srl C
            unlessFlag NC $ add HL DE
            jp Z end
            sla E
            rl D

        -- Add X / 2 to HL, aligned to 8 pixels since IX points to a whole byte
        pop AF
        replicateM_ 1 rrca
        Z80.and 0x1c
        ld D 0
        ld E A
        add HL DE

        push IY
        srl B
        inc B
        withLabel \loopRow -> do
            ld D 0

            call renderColumns

            ld DE $ fromIntegral pictureWidth `div` 8 + (fromIntegral pictureWidth `div` 8 - 2)
            add IX DE
            ld DE $ numCols - 8
            add HL DE

            djnz loopRow

        pop IY
        ret

        renderColumns <- labelled do
            call renderColumn
            ld A [isLastColumn]
            Z80.and A
            jp Z renderColumn

            ld DE 4
            add HL DE
            inc IX
            ret

        renderColumn <- labelled do
            push BC
            decLoopB 4 do
                -- Calculate into E the next 2x2 bit pattern
                ld E 0

                ld A [IX + 0]
                rlca
                rl E
                rlca
                rl E
                ld [IX + 0] A

                ld A [IX + fromIntegral (pictureWidth `div` 8)]
                rlca
                rl E
                rlca
                rl E
                ld [IX + fromIntegral (pictureWidth `div` 8)] A

                -- Convert bit pattern into character
                ld IY charmap
                add IY DE
                ldVia A [HL] [IY]
                inc HL
            pop BC
            inc IX
            ret
        pure ()


    -- Scan the keyboard and write its state to the 16 bytes starting at `kbdBuf`
    scanKeys <- labelled do
        let keymap = keymapHL2
            keymapSorted = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ [(addr, (bit, value)) | (value, (addr, bit)) <- zip [0..] keymap]
        forM_ keymapSorted \(keys@((addr, _):_)) -> do
            ld A [addr]
            forM_ keys \(_, (i, value)) -> do
                ld HL $ kbdBuf + value
                ld [HL] 0x00
                Z80.bit i A
                unlessFlag NZ $ dec [HL]
        ret

    -- Wait for keypress, write its code into `B`
    waitKeyPress <- labelled mdo
        loopForever do
            -- Store old keyboard state
            ld DE oldState
            ld HL kbdBuf
            ld BC 16
            ldir

            call scanKeys
            ld HL kbdBuf
            ld DE oldState
            ld B 0
            withLabel \loop -> do
                -- Check for a key that wasn't pressed before, but is pressed now
                ld A [DE]
                inc DE
                cpl
                ld C [HL]
                inc HL
                Z80.and C
                ret NZ

                inc B
                ld A B
                cp 16
                jp NZ loop
        oldState <- labelled $ db $ replicate 16 0
        pure ()

    -- If key in `A` is pressed, set `Z`
    checkKey <- labelled do
        call scanKeys
        ld B 0
        ld C A
        ld HL kbdBuf
        add HL BC
        ld A [HL]
        cp 0xff
        ret

    charmap <- labelled $ db charmapHL2

    hex <- labelled $ db $
      let rowToBits = foldr (\c b -> let b' = b `shiftR` 1 in if c == ' ' then b' else setBit b' 7) 0
          pixelsOf rows = [rowToBits row | row <- rows] ++ [0x00, 0x00, 0x00]
      in foldMap pixelsOf font
    pure ()

charmapHL4 :: [Word8]
charmapHL4 =
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

charmapHL2 :: [Word8]
charmapHL2 =
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

keymapHL2 :: [(Location, Word8)]
keymapHL2 =
    [ (0x3a7f, 0) -- X

    , (0x3afb, 1) -- 1
    , (0x3afb, 2) -- 2
    , (0x3afb, 3) -- 3

    , (0x3abf, 1) -- Q
    , (0x3abf, 7) -- W
    , (0x3aef, 5) -- E

    , (0x3aef, 1) -- A
    , (0x3abf, 3) -- S
    , (0x3aef, 4) -- D

    , (0x3a7f, 2) -- Z
    , (0x3aef, 3) -- C
    , (0x3afb, 4) -- 4
    , (0x3abf, 2) -- R
    , (0x3aef, 6) -- F
    , (0x3abf, 6) -- V
    ]

font :: [[String]]
font =
    [ [ "####"
      , "#  #"
      , "#  #"
      , "#  #"
      , "####"
      ]
    , [ "  # "
      , " ## "
      , "  # "
      , "  # "
      , " ###"
      ]
    , [ "####"
      , "   #"
      , "####"
      , "#   "
      , "####"
      ]
    , [ "####"
      , "   #"
      , "####"
      , "   #"
      , "####"
      ]
    , [ "#  #"
      , "#  #"
      , "####"
      , "   #"
      , "   #"
      ]
    , [ "####"
      , "#   "
      , "####"
      , "   #"
      , "####"
      ]
    , [ "####"
      , "#   "
      , "####"
      , "#  #"
      , "####"
      ]
    , [ "####"
      , "   #"
      , "  # "
      , " #  "
      , " #  "
      ]
    , [ "####"
      , "#  #"
      , "####"
      , "#  #"
      , "####"
      ]
    , [ "####"
      , "#  #"
      , "####"
      , "   #"
      , "####"
      ]
    , [ " ## "
      , "#  #"
      , "####"
      , "#  #"
      , "#  #"
      ]
    , [ "### "
      , "#  #"
      , "### "
      , "#  #"
      , "### "
      ]
    , [ "####"
      , "#   "
      , "#   "
      , "#   "
      , "####"
      ]
    , [ "### "
      , "#  #"
      , "#  #"
      , "#  #"
      , "### "
      ]
    , [ "####"
      , "#   "
      , "####"
      , "#   "
      , "####"
      ]
    , [ "####"
      , "#   "
      , "### "
      , "#   "
      , "#   "
      ]
    ]
