module Target.GameBoy.Video where

import Target.GameBoy.Operations
import Z80 hiding (decLoopB)
import Z80.Utils
import Data.Word

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
    ld DE 0x9000
    ld HL vidbuf

    -- Copy 4 8-row units
    decLoopC 4 do
        -- Copy 8 rows
        decLoopB 8 do
            push BC

            -- Copy one byte into [DE]
            decLoopB 8 do
                ld A [HLi]
                ld [DE] A
                inc DE
                ld [DE] A

                -- Add 15 to DE
                ld A E
                add A 15
                ld E A
                unlessFlag NC $ inc D

            -- Jump to next row
            ld A E
            sub (128 - 2)
            ld E A
            unlessFlag NC $ dec D

            pop BC

        ld A E
        add A (128 - 16)
        ld E A
        unlessFlag NC $ inc D
