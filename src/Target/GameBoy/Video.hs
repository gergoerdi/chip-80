module Target.GameBoy.Video where

import Target.GameBoy.Operations
import Z80
import Z80.Utils
import Control.Monad (replicateM_)

renderToTiles :: Location -> Z80ASM
renderToTiles vidbuf = do
    ld DE 0x9000
    ld HL vidbuf

    -- 8 rows of tiles
    ld C 8
    withLabel \loop -> do
        -- 8 pixel lines per tile
        ld B 8
        withLabel \loop -> do
            push BC

            -- 8 double tiles per row
            ld C 8
            withLabel \loop -> do
                push BC

                ld A [HLi]

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
                    ldVia A [DE] C

                    -- Add 16 to DE to jump to next tile
                    ld A E
                    add A 16
                    ld E A
                    unlessFlag NC $ inc D
                    pop AF

                pop BC
                dec C
                jp NZ loop

            -- End of row: go back to second line of first tile
            ld A E
            sub 254
            ld E A
            unlessFlag NC $ dec D

            pop BC
            dec B
            jp NZ loop

        -- End of tile row: go to first line of next row
        ld A E
        add A (256 - 16)
        ld E A
        unlessFlag NC $ inc D

        dec C
        jp NZ loop
