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

blitTiles :: Location -> Z80ASM
blitTiles tilebuf = do
    ld HL tilebuf
    ld DE 0x9000

    decLoopB 128 do
        decLoopC 8 do
            ld A [HLi]
            ld [DE] A
            inc DE
            inc DE

renderToTiles :: Location -> Location -> Z80ASM
renderToTiles vidbuf tilebuf = do
    ld HL tilebuf
    ld DE vidbuf

    -- 8 rows of tiles
    decLoopC 8 do
        -- 8 pixel lines per tile
        decLoopB 8 do
            push BC

            -- 8 double tiles per row
            decLoopC 8 do
                push BC

                ld A [DE]
                inc DE

                replicateM_ 2 do
                    -- Spread high nybble into C
                    ld C 0
                    decLoopB 4 do
                        rla
                        push AF
                        rl C
                        pop AF
                        rl C

                    push AF
                    ldVia A [HL] C

                    -- Add 16 to DE to jump to next tile
                    ld A L
                    add A 8
                    ld L A
                    unlessFlag NC $ inc H
                    pop AF

                pop BC

            -- End of row: go back to second line of first tile
            ld A L
            sub (128 - 1)
            ld L A
            unlessFlag NC $ dec H

            pop BC

        -- End of tile row: go to first line of next row
        ld A L
        add A (128 - 8)
        ld L A
        unlessFlag NC $ inc H
