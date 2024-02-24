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

renderToTiles :: Location -> Z80ASM
renderToTiles vidbuf = do
    ld HL 0x9000
    ld DE vidbuf

    -- 4 double rows of tiles
    decLoopC 4 do
        -- 8 pixel lines per tile
        ld B 8

        push HL
        withLabel \loop -> do
            push BC

            push HL

            -- 8 double tiles per row
            decLoopC 8 do
                push BC

                ld A [DE]
                inc DE

                replicateM_ 2 do
                    -- Spread high nybble into C
                    ld C 0
                    ld B 4
                    withLabel \loop -> do
                        rla
                        push AF
                        rl C
                        pop AF
                        rl C

                        dec B
                        jp NZ loop

                    push AF
                    ld A C
                    ld [HLi] A
                    inc HL
                    ld [HL] A

                    -- Add 14 to HL to jump to next tile
                    ld A L
                    add A (32 - 2)
                    ld L A
                    unlessFlag NC $ inc H
                    pop AF

                pop BC

            -- End of row: go back to next line of first tile
            pop HL
            replicateM_ 4 $ inc HL

            pop BC
            dec B
            jp NZ loop

        -- End of tile row: go to first line of next row
        pop HL
        ld A H
        add A 0x02
        ld H A
