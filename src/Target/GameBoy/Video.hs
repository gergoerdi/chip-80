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

    -- Copy 4 8-row units
    decLoopC 4 do
        -- Copy 8 rows
        decLoopB 8 do
            push BC

            -- Copy one byte into [HL]
            decLoopB 8 do
                ld A [DE]
                inc DE
                ld [HLi] A

                -- Add 7 to DE
                ld A L
                add A 7
                ld L A
                unlessFlag NC $ inc H

            -- Jump to next row
            ld A L
            sub (64 - 1)
            ld L A
            unlessFlag NC $ dec H

            pop BC

        ld A L
        add A (64 - 8)
        ld L A
        unlessFlag NC $ inc H
