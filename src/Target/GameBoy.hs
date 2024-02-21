module Target.GameBoy where

import Target.GameBoy.Operations
import Z80
import Z80.Utils

import qualified Data.ByteString as BS
import Data.String (fromString)
import System.FilePath
import System.Directory
import Data.Word

emit :: IO ()
emit = do
    let prog = org 0x0100 mdo
            jp start

            -- Leave room for the header
            db $ replicate (0x050 - 3) 0x00

            start <- label
            -- di
            withLabel \waitVBlank -> do
                ldh A [0x44]
                cp 144
                jr C waitVBlank

            -- Turn off LCD
            ldhVia A [0x40] 0b0000_0000

            let copyHLtoDE = withLabel \loop -> do
                    ldVia A [DE] [HL] -- TODO: [HLi]
                    inc DE
                    inc HL
                    dec BC
                    ld A B
                    Z80.or C
                    jp NZ loop

            -- Tile definitions
            ld DE 0x9000
            ld HL tiles
            ld BC $ 64 `div` 8 * 32 `div` 8 * 16
            copyHLtoDE

            -- Tilemap
            ld DE 0x9800
            ld A 0x00
            withLabel \loop -> do
                ld [DE] A
                inc DE
                inc A
                cp 32
                jp C loop

            -- Scrolling
            ldhVia A [0x43] 0
            ldhVia A [0x42] 0

            -- Turn on LCD
            ldhVia A [0x40] 0b1000_0001

            ldhVia A [0x47] 0b1110_0100
            loopForever $ pure ()

            tiles <- labelled $ dw $ replicate (64 `div` 8 * 32 `div` 8 * 8) 0b0000_0000_0000_0000
            pure ()

    let name = "_build/chip80-gameboy"
    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData prog
    BS.writeFile (name <.> "gb") $ gb prog

gb :: ASMBlock -> BS.ByteString
gb prog = BS.take (8 * 1024) $ mconcat
    [ BS.replicate (fromIntegral $ asmOrg prog) 0x00
    , asmData prog
    , BS.replicate (8 * 1024) 0x00
    ]
