module Target.GameBoy.Video where

import Target.GameBoy.Operations
import Z80 hiding (decLoopB)
import Z80.Utils
import Data.Word
import Control.Monad (replicateM_)

decLoopB :: Word8 -> Z80ASM -> Z80ASM
decLoopB n body = do
    ld B n
    withLabel \loop -> do
        body
        dec B
        jp NZ loop

decLoopC :: Word8 -> Z80ASM -> Z80ASM
decLoopC n body = do
    ld C n
    withLabel \loop -> do
        body
        dec C
        jp NZ loop

blitAllTiles :: Location -> Z80ASM
blitAllTiles tilebuf = do
    ld HL tilebuf
    ld DE 0x9000

    decLoopB 0x80 do
        decLoopC 8 do
            ld A [HLi]
            ld [DE] A
            inc DE
            inc DE

-- | Pre: `A` contains X coordinate, `C` contains Y coordinate
--   Post: `HL` contains tilebuf starting address, `DE` contains tile data starting address
prepareDirtyTiles :: Location -> Z80ASM
prepareDirtyTiles tilebuf = do
    -- CHIP-80 coordinate system: 64x32 = 8x32 * 8x1 (row of 8 pixels / byte)
    -- Tilemap coordinate system: 128x64 = 16x8 * 8x8
    --
    -- Tilemap:
    --
    -- 0x00 0x01 ... 0x0f
    -- 0x10 0x11 ... 0x1f
    -- ...
    -- 0x70 0x71 ... 0x7f
    --
    -- Each 2x2 "supertile" contains 8x8 CHIP-8 pixels.
    --
    -- CHIP-80 tile index = Y << 3 | X / 8
    -- GB tile index = (Y / 4) << 4 | X / 4
    --
    -- Starting from there, we invalidate 4x(height / 2) tiles

    -- Calculate tile index into E
    replicateM_ 2 $ srl A -- A /= 4
    replicateM_ 2 $ srl C -- C /= 4
    replicateM_ 4 $ sla C -- C <<= 4
    add A C
    ld E A

    -- Calculate offset = 16 * E into DE
    ld D 0x00
    replicateM_ 4 do
        sla E
        rl D

    -- Calculate source HL = tilebuf + offset
    ld HL tilebuf
    add HL DE

    -- Calculate target DE = 0x9000 + offset
    ld A D
    add A 0x90
    ld D A


-- | Pre: stack contains five start addresses of five tilebuf rows
--   This needs to be fast...
blitDirtyTiles :: Z80ASM
blitDirtyTiles = do
    -- Copy five tiles vertically
    decLoopB 5 do
        pop DE
        pop HL

        -- Copy three tiles horizontally
        decLoopC 3 $ replicateM_ 8 do
            ld A [HLi]
            ld [DE] A
            inc DE
            inc DE

renderToTiles :: Location -> Location -> Z80ASM
renderToTiles vidbuf tilebuf = do
    ld HL tilebuf
    ld DE vidbuf

    -- 32 doubled pixel rows total, over 8 tiles
    decLoopB 8 do
        push BC
        push HL

        -- One tile row: 4 vertically doubled pixel rows
        decLoopB 4 do
            push HL

            -- One pixel row: 8 doubled tiles
            decLoopC 8 do
                push BC

                ld A [DE]
                inc DE

                replicateM_ 2 do
                    -- Spread one nybble of A into C
                    decLoopB 4 do
                        rla
                        push AF
                        rl C
                        pop AF
                        rl C

                    -- Store C twice to double vertically
                    push AF
                    ld A C
                    ld [HLi] A
                    ld [HL] A
                    pop AF

                    -- Jump to the same row of the next tile
                    ld BC (8 - 1)
                    add HL BC

                pop BC

            pop HL
            replicateM_ 2 $ inc HL

        pop HL
        ld BC (16 * 8)
        add HL BC
        pop BC
