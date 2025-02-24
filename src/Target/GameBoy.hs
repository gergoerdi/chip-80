module Target.GameBoy where

import Target.GameBoy.Operations
import Z80 hiding (decLoopB)
import Z80.Utils

import Target.GameBoy.Video

import Control.Monad (replicateM_)

import qualified Data.ByteString as BS
import Data.String (fromString)
import System.FilePath
import System.Directory
import Data.Word

withHeader :: Z80ASM -> ASMBlock
withHeader prog = org 0x0100 mdo
    jp start

    -- Pad to 4 bytes before header
    db [0x00]

    -- Header
    db $ take (0x050 - 4) . (<> repeat 0x00) . mconcat $
        -- Boot logo
        [ [ 0xce, 0xed, 0x66, 0x66, 0xcc, 0x0d, 0x00, 0x0b, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0c, 0x00, 0x0d
          , 0x00, 0x08, 0x11, 0x1f, 0x88, 0x89, 0x00, 0x0e, 0xdc, 0xcc, 0x6e, 0xe6, 0xdd, 0xdd, 0xd9, 0x99
          , 0xbb, 0xbb, 0x67, 0x63, 0x6e, 0x0e, 0xec, 0xcc, 0xdd, 0xdc, 0x99, 0x9f, 0xbb, 0xb9, 0x33, 0x3e
          ]
        -- Game title
        , replicate 16 0x00

        -- New licensee code
        , [ 0x00, 0x00 ]

        -- SGB flag
        , [ 0x00 ]

        -- Cartridge type
        , [ 0x00 ]

        -- ROM size
        , [ 0x00 ]

        -- RAM size
        , [ 0x00 ]

        -- Destination code
        , [ 0x00 ]

        -- Old licensee code
        , [ 0x00 ]

        -- Mask ROM version number
        , [ 0x00 ]

        -- TODO: header checksum
        , [ 0xe7 ]

        -- TODO: global checksum
        , [ 0xfc, 0xd8 ]
        ]


    start <- label
    prog

emit :: IO ()
emit = do
    let prog = withHeader mdo
            -- di
            withLabel \waitVBlank -> do
                ldh A [0x44]
                cp 144
                jr C waitVBlank

            -- Turn off LCD
            ldhVia A [0x40] 0b0000_0000

            let copyHLtoDE = withLabel \loop -> do
                    ldVia A [DE] [HLi]
                    inc DE
                    dec BC
                    ld A B
                    Z80.or C
                    jp NZ loop

            -- Initialize tiles to empty
            ld HL 0x9000
            ld BC 0x800
            withLabel \loop -> do
                ld A 0x00
                ld [HLi] A
                dec BC
                ld A 0x98
                cp H
                jp NZ loop

            -- TEMP: Set empty tile to a pattern for debugging
            ld A 0b1100_1100
            ld HL $ 0x8800
            decLoopB 16 do
                ld [HLi] A

            -- Tilemap
            ld HL 0x9800

            -- Fill screen with 0x80
            let tilemapW, tilemapH :: (Num a) => a
                tilemapW = 32
                tilemapH = 32
            ld A 0x80
            decLoopC tilemapH do
                decLoopB tilemapW do
                    ld [HLi] A

            -- Fill CHIP-80 area with 0x00..0x7f
            let visibleW = 20
                visibleH = 18
                chipW, chipH :: (Integral a) => a
                chipW = (64 * 2) `div` 8
                chipH = (32 * 2) `div` 8
            ld HL $ 0x9800 + (((visibleH - chipH) `div` 2) - 4) * tilemapW + (visibleW - chipW) `div` 2
            ld A 0x00
            ld DE (tilemapW - chipW)
            decLoopC chipH do
                decLoopB chipW do
                    ld [HLi] A
                    inc A
                add HL DE

            -- Reset scrolling
            ldhVia A [0x43] 0
            ldhVia A [0x42] 0

            -- Initialize vidbuf
            ld DE vidbuf
            ld HL picdata
            ld BC $ 64 * 32 `div` 8
            copyHLtoDE

            renderToTiles vidbuf tilebuf
            blitAllTiles tilebuf

            -- TEMP: Set tilebuf to all black, so we can see the effect of blitDirtyTiles
            ld A 0xff
            ld HL tilebuf
            decLoopB 256 do
                decLoopC 8 do
                    ld [HLi] A

            -- Turn on LCD
            ldhVia A [0x40] 0b1000_0001

            ldhVia A [0x47] 0b1110_0100

            ld HL curX
            ld [HL] 0x0
            ld HL curY
            ld [HL] 0x00

            loopForever do
                decLoopB 6 do
                    withLabel \waitVBlank -> do
                        ldh A [0x44]
                        cp 144
                        jr NZ waitVBlank
                    withLabel \waitVBlank -> do
                        ldh A [0x44]
                        cp 144
                        jr Z waitVBlank

                ldVia A B 15

                ld HL curX
                ld A [HL]
                inc [HL]

                ld HL curY
                ld C [HL]
                -- inc [HL]

                cp 32
                unlessFlag NZ $ loopForever nop

                prepareDirtyTiles tilebuf
                ld BC (16 * 8)

                push HL
                push DE
                replicateM_ 4 do
                    -- HL += BC
                    add HL BC
                    push HL

                    -- DE += 2 * bc
                    push HL
                    push DE; pop HL
                    add HL BC
                    add HL BC
                    push HL; pop DE
                    pop HL
                    push DE

                withLabel \waitVBlank -> do
                    ldh A [0x44]
                    cp 144
                    jr NZ waitVBlank

                blitDirtyTiles

            picdata <- labelled $ db $ mconcat picture
            let vidbuf = 0xc000
                curX = vidbuf + 0x0800
                curY = curX + 1
            let tilebuf = 0xd000

            pure ()

    let name = "_build/chip80-gameboy"
    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData prog
    BS.writeFile (name <.> "gb") $ gb prog

gb :: ASMBlock -> BS.ByteString
gb prog = BS.take (32 * 1024) $ mconcat
    [ BS.replicate (fromIntegral $ asmOrg prog) 0x00
    , asmData prog
    , BS.replicate (32 * 1024) 0x00
    ]

picture :: [[Word8]]
picture =
    [ [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x0f, 0xa0, 0x00, 0x01, 0x00, 0x30, 0x00 ]
    , [ 0x00, 0x02, 0x0d, 0x19, 0xc7, 0x49, 0x90, 0x00 ]
    , [ 0x00, 0x02, 0x2a, 0xa5, 0x29, 0x4a, 0x00, 0x00 ]
    , [ 0x00, 0x02, 0x28, 0xbd, 0x29, 0x49, 0x00, 0x00 ]
    , [ 0x00, 0x02, 0x28, 0xa1, 0x29, 0x48, 0x80, 0x00 ]
    , [ 0x00, 0x02, 0x28, 0x9d, 0x27, 0x3b, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x1f, 0x18, 0x0c, 0xf8, 0x00, 0xfe, 0x00 ]
    , [ 0x00, 0x3f, 0xb8, 0x1d, 0xfc, 0x01, 0xc7, 0x00 ]
    , [ 0x00, 0x71, 0xb8, 0x1d, 0xce, 0x03, 0x83, 0x00 ]
    , [ 0x00, 0xe0, 0x38, 0x01, 0xc6, 0x03, 0x83, 0x00 ]
    , [ 0x00, 0xe5, 0x38, 0x0d, 0xc6, 0x03, 0x83, 0x00 ]
    , [ 0x00, 0xe0, 0x3f, 0x1d, 0xc6, 0x01, 0xc6, 0x00 ]
    , [ 0x00, 0xe8, 0xbf, 0x9d, 0xc6, 0xf0, 0xfc, 0x00 ]
    , [ 0x00, 0xe7, 0x39, 0xdd, 0xce, 0xf1, 0xce, 0x00 ]
    , [ 0x00, 0xe0, 0x38, 0xdd, 0xfc, 0x03, 0x87, 0x00 ]
    , [ 0x00, 0xe0, 0x38, 0xdd, 0xf8, 0x07, 0x03, 0x00 ]
    , [ 0x00, 0xe0, 0x38, 0xdd, 0xc0, 0x07, 0x03, 0x00 ]
    , [ 0x00, 0xe0, 0x38, 0xdd, 0xd4, 0x27, 0x03, 0x00 ]
    , [ 0x00, 0x71, 0xb8, 0xdd, 0xdc, 0x67, 0x87, 0x00 ]
    , [ 0x00, 0x3f, 0xb8, 0xdd, 0xc4, 0x23, 0xfe, 0x00 ]
    , [ 0x00, 0x1f, 0x38, 0xdd, 0xc5, 0x71, 0xfc, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x07, 0x31, 0xa0, 0x30, 0x28, 0x60, 0x00 ]
    , [ 0x00, 0x02, 0x4a, 0x38, 0x44, 0x8e, 0x90, 0x00 ]
    , [ 0x00, 0x02, 0x79, 0x20, 0x24, 0xa8, 0xf0, 0x00 ]
    , [ 0x00, 0x02, 0x40, 0xa0, 0x14, 0xa8, 0x80, 0x00 ]
    , [ 0x00, 0x02, 0x3b, 0x18, 0x63, 0xa6, 0x70, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]
