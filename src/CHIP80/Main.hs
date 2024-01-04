{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module CHIP80.Main (game) where

import HL2
import CHIP80.CPU
import CHIP80.Font
import CHIP80.HL2.Input
import CHIP80.HL2.Video

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

game :: BS.ByteString -> Z80ASM
game image = mdo
    let baseAddr = 0x7000
    ld SP $ baseAddr - (256 + 16) - 1

    -- Load program into CHIP-8 RAM
    ld HL prog
    ld DE $ baseAddr + 0x200
    ld BC $ fromIntegral $ BS.length image -- (0x1000 - 0x200)
    ldir

    ld IY $ baseAddr + 0x1ff
    ld [IY] 1

    run baseAddr
    prog <- labelled $ db image
    pure ()

setup :: Location -> Z80ASM
setup baseAddr = mdo
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

run :: Location -> Z80ASM
run baseAddr = mdo
    let vidBuf = baseAddr - 256
        keyBuf = vidBuf - 16

    ld IY $ baseAddr + 0x200
    loopForever do
        call cpu
        ld HL lastFrame
        ld A [0x403f]
        cp [HL]
        unlessFlag Z do
            ld [lastFrame] A
            call scanKeys
            call newFrame

    timer <- labelled $ db [0]
    lastFrame <- labelled $ db [0]
    waitForFrame <- labelled $ db [0]

    let platform = Platform{ vidAddr = vidBuf, .. }
    cpu <- labelled $ cpu_ def platform
    newFrame <- labelled $ newFrame_ platform

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
