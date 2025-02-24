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
