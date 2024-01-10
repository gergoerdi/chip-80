{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module CHIP80.HL2.Machine (machine_) where

import HL2
import CHIP80.CPU
import CHIP80.Quirks
import CHIP80.Font
import CHIP80.HL2.Input
import CHIP80.HL2.Video
import Z80.ZX0

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.ByteString as BS
import Data.List (sortBy, groupBy, intercalate)
import Data.Function (on)

-- | Pre: `IX` contains address of quirks settings
-- | Pre: `IY` contains address of compressed program
machine_ :: Location -> Z80ASM
machine_ baseAddr = mdo
    call drawUI
    call setup

    ldVia A [shiftVY] [IX + 0]
    ldVia A [resetVF] [IX + 1]
    ldVia A [incrementPtr] [IX + 2]
    ldVia A [videoWait] [IX + 3]
    ldVia A [clipSprites] [IX + 4]

    -- Uncompress program into CHIP-8 RAM
    push IY
    pop HL
    ld DE $ baseAddr + 0x200
    call uncompress

    run quirks baseAddr

    drawUI <- labelled drawUI_
    setup <- labelled $ setup_ baseAddr
    uncompress <- labelled standardFwd
    quirks@Quirks{..} <- do
        shiftVY <- labelled $ db [1]
        resetVF <- labelled $ db [1]
        incrementPtr <- labelled $ db [1]
        videoWait <- labelled $ db [1]
        clipSprites <- labelled $ db [1]
        pure Quirks{..}

    pure ()

drawUI_ :: Z80ASM
drawUI_ = mdo
    -- Clear screen
    ld HL videoStart
    withLabel \loop -> do
        ld [HL] 0x20
        inc HL
        ld A H
        cp 0xc4
        jp NZ loop

    ld HL $ videoStart + 4 + (3 - 1) * 40 - 1
    ld [HL] 0x6e
    inc HL
    ld A 0x96
    decLoopB 32 do
        ld [HL] A
        inc HL
    ld [HL] 0x6d

    ld HL $ videoStart + 4 + (3 + 16) * 40 - 1
    ld [HL] 0x6c
    inc HL
    ld A 0x95
    decLoopB 32 do
        ld [HL] A
        inc HL
    ld [HL] 0x6b

    ld HL $ videoStart + 4 + 3 * 40 - 1
    decLoopB 16 do
        ld [HL] 0xeb
        ld DE 33
        add HL DE
        ld [HL] 0xea
        ld DE 7
        add HL DE

    -- Draw main UI
    ld HL $ videoStart + 40
    ld DE banner
    skippable \end -> loopForever do
        ld A [DE]
        Z80.and A
        jp Z end
        ld [HL] A
        inc HL
        inc DE
    ld HL $ videoStart + (3 + 16 + 1 + 1) * 40
    forM_ keyss \keys -> do
        ld DE keys
        skippable \end -> loopForever do
            ld A [DE]
            Z80.and A
            jp Z end
            ld [HL] A
            inc HL
            inc DE
        ld DE (40 - 10) -- 4 * 4 + 1)
        add HL DE
    ld HL $ videoStart + (3 + 16 + 1 + 1) * 40 + 20
    ld DE reset
    skippable \end -> loopForever do
        ld A [DE]
        Z80.and A
        jp Z end
        ld [HL] A
        inc HL
        inc DE
    ret
    banner <- labelled $ db $ (++ [0]) $ map (fromIntegral . ord . toUpper) $ invert "   CHIP-80     https://gergo.erdi.hu/   "
    keyss <- mapM (labelled . db . (++ [0]) . map (fromIntegral . ord . toUpper)) $
      -- let rows = [ [ ('1', '1'), ('2', '2'), ('3', '3'), ('C', '4') ]
      --            , [ ('4', 'q'), ('5', 'W'), ('6', 'e'), ('D', 'r') ]
      --            , [ ('7', 'a'), ('8', 's'), ('9', 'd'), ('E', 'f') ]
      --            , [ ('A', 'z'), ('0', 'x'), ('B', 'c'), ('F', 'v') ]
      --            ]
      -- in [ intercalate " " [ [sym, ' ', invert1 key] | (sym, key) <- row ] | row <- rows ]
      let rows = [ ("123C", "1234")
                 , ("456D", "QWER")
                 , ("789E", "ASDF")
                 , ("A0BF", "ZXCV")
                 ]
      in [ sym ++ "  " ++ invert key | (sym, key) <- rows ]
    reset <- labelled . db . (++ [0]) . map (fromIntegral . ord . toUpper) $
        invert "RUN/BRK" <> ": Change game"
    pure ()

setup_ :: Location -> Z80ASM
setup_ baseAddr = mdo
    -- Zero out CHIP-8 RAM
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
    ret

    hex <- labelled $ db font
    pure ()

run :: Quirks Location -> Location -> Z80ASM
run quirks baseAddr = mdo
    let vidBuf = baseAddr - 256
        keyBuf = vidBuf - 16

    reset locs platform

    ld IY $ baseAddr + 0x200
    loopForever do
        call cpu
        ld HL lastFrame
        ld A [0x403f]
        cp [HL]
        unlessFlag Z do
            ld [lastFrame] A
            call scanKeys
            ret Z
            call newFrame
    lastFrame <- labelled $ db [0]

    let platform = Platform{ vidAddr = vidBuf, .. }
    locs <- allocate
    cpu <- labelled $ cpu_ quirks locs platform
    newFrame <- labelled $ newFrame_ locs platform

    clearScreen <- labelled do
        ld HL $ videoStart + 4 + 40 * 3
        ld DE 8
        decLoopB 16 do
            ld C B
            decLoopB 32 do
                ld [HL] 0x20
                inc HL
            add HL DE
            ld B C
        ret

    spritePre <- labelled do
        ld [spriteX] A
        push AF
        ldVia A [spriteY] C
        ldVia A [spriteH] B
        pop AF
        ret

    spritePost <- label
    ld A 0x00
    spriteX <- subtract 1 <$> label
    ld C 0x00
    spriteY <- subtract 1 <$> label
    ld B 0x00
    spriteH <- subtract 1 <$> label
    drawSprite vidBuf

    -- Scan the keyboard and write its state to the 16 bytes starting at `keyBuf`
    scanKeys <- labelled $ scanKeys_ keyBuf
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
