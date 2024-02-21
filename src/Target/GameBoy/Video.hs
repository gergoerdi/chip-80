module Target.GameBoy.Video where

import Target.GameBoy.Operations
import Z80
import Z80.Utils

renderToTiles :: Location -> Z80ASM
renderToTiles vidbuf = do
    ld DE 0x9000
    ld HL vidbuf

    -- Copy 4 8-row units
    ld C 4
    withLabel \loop -> do
        -- Copy 8 rows
        ld B 8
        withLabel \loop -> do
            push BC

            -- Copy one byte into [DE]
            ld B 8
            withLabel \loop -> do
                ld A [HL]
                inc HL
                ld [DE] A
                inc DE
                ld [DE] A

                -- Add 15 to DE
                ld A E
                add A 15
                ld E A
                unlessFlag NC $ inc D

                dec B
                jp NZ loop

            -- Jump to next row
            ld A E
            sub (128 - 2)
            ld E A
            unlessFlag NC $ dec D

            pop BC
            dec B
            jp NZ loop

        ld A E
        add A (128 - 16)
        ld E A
        unlessFlag NC $ inc D

        dec C
        jp NZ loop
