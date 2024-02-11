module Target.HomeLab.HL4.Shell (withGamesFrom) where

import Z80.Machine.HomeLab.HL34
import CHIP80.Quirks
import CHIP80.Game
import Target.HomeLab.HL4.Machine
import Target.HomeLab.HL4.Video64Small (encodeFromPng)

import Z80
import Z80.Utils
import Z80.ZX0
import Z80.ZX0.Compress
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.ByteString as BS
import Data.List (sortBy, groupBy, intercalate)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.String (fromString)

import Data.Yaml
import Data.Aeson (fromJSON, Result(..))
import Data.Aeson.KeyMap as KeyMap (toList, lookup)
import Data.Aeson.Key (toString)
import Data.Text (unpack)
import System.FilePath
import Text.Printf

withGamesFrom :: FilePath -> IO Z80ASM
withGamesFrom dir = do
    logo <- BS.readFile (dir </> "logo-url.png")
    selected <- decodeFileThrow (dir </> "hl4.yaml")
    games <- readGames selected (dir </> "games.yaml")
    pure $ shell games logo

shell :: [Game] -> BS.ByteString -> Z80ASM
shell games logo = mdo
    di

    -- Restore output vector
    ld HL 0x0283
    ld [0x4004] HL

    -- Restore input vector
    ld HL 0x035c
    ld [0x4002] HL

    let baseAddr = 0xb000
    ld SP $ baseAddr - 1

    loopForever do
        ld IY 0x4000 -- Printing doesn't work unless IY is restored
        pageIO

        -- Clear screen
        ld A 0x00
        ld DE videoStart
        decLoopB 32 do
            ld C B
            decLoopB 64 do
                ld [DE] A
                inc DE
            ld B C

        -- Reset cursor position
        ld HL videoStart
        ld [0x4014] HL

        let videoAt (row, col) = videoStart + rowstride * row + col

        ld DE $ videoAt (2, (rowstride - fromIntegral logoWidth) `div` 2 - 3)
        ld HL progCopy
        call print

        ld DE $ videoAt (2 + fromIntegral logoHeight + 2, 38)
        ld HL logoCopy
        call print

        ld DE $ videoAt (4, (rowstride - fromIntegral logoWidth) `div` 2)
        ld HL logoData
        decLoopB logoHeight do
            push BC
            ld BC $ fromIntegral logoWidth
            ldir
            ld BC $ fromIntegral $ rowstride - logoWidth
            ex DE HL
            add HL BC
            ex DE HL
            pop BC

        ld DE $ videoAt (31, 6)
        ld HL instructions
        call print

        -- Print menu of available programs
        ld IX titleTable

        -- First column: first ten games
        ld C $ fromIntegral . ord $ '0' -- labelled '0'..'9'
        ld DE $ videoAt (20, 0)
        ld A [numProgs]
        cp 10
        unlessFlag C $ ld A 10
        ld B A
        call printMenuColumn

        -- Print second column, if we have more than 10 entries
        ld A [numProgs]
        sub 10
        unlessFlag C do
            ld B A
            ld C $ fromIntegral . ord $ 'A' -- labelled 'A'..
            ld DE $ videoAt (20, 32)
            call printMenuColumn

        withLabel \inputLoop -> mdo
            getKeyA

            tryDigit <- labelled $ do
                cp $ fromIntegral . ord $ '0'
                jp C tryLetter

                cp $ succ . fromIntegral . ord $ '9'
                jp NC tryLetter

                sub $ fromIntegral . ord $ '0'
                cp (fromIntegral $ length progs + 1)
                jp NC inputLoop
                jp found

            tryLetter <- labelled do
                cp $ fromIntegral . ord $ 'A'
                jp C inputLoop

                sub $ fromIntegral . ord $ 'A'
                add A 10
                cp (fromIntegral $ length progs + 1)
                jp NC inputLoop
                jp found

            found <- labelled do
                ld D 0
                sla A
                ld E A

                let derefTableBC table = do
                        ld HL table
                        add HL DE
                        ld C [HL]
                        inc HL
                        ld B [HL]

                derefTableBC progTable
                push BC
                pop IX

                derefTableBC joyTable
                push BC
                pop IY

            call machine

    printMenuColumn <- labelled do
        withLabel \loop -> do
            printLn do
                ld L [IX]
                inc IX
                ld H [IX]
                inc IX

                space
                ld A C
                inc C
                printA
                space

                call print
            djnz loop
        ret

    -- `DE`: destination, `HL`: start of 0-delimited string
    print <- labelled do
        loopForever do
            ld A [HL]
            inc HL
            Z80.and A
            ret Z
            ld [DE] A
            inc DE

    let space = inc DE
        printA = do
            ld [DE] A
            inc DE

    let printLn body = do
            push DE
            body
            pop DE
            ld HL 64
            add HL DE
            ex DE HL

    let (logoWidth, logoHeight, logoBytes) = encodeFromPng logo
    logoData <- labelled $ db logoBytes
    progCopy <- labelled $ db $ (<> [0]) . map (fromIntegral . ord) $
        "Gergo Erdi's"
    logoCopy <- labelled $ db $ (<> [0]) . map (fromIntegral . ord) $
        "Logo by Tim Franssen"
    instructions <- labelled $ db $ (<> [0]) . map (fromIntegral . ord) $
        "1234/QWER/ASDF/ZXCV for input, Return (CR) to reset"

    machine <- labelled $ machine_ baseAddr

    numProgs <- labelled $ db [fromIntegral $ length progs]
    titleTable <- labelled $ dw [ title | (title, _, _) <- progs ]
    joyTable <- labelled $ dw [ joy | (_, joy, _) <- progs ]
    progTable <- labelled $ dw [ prog | (_, _, prog) <- progs ]

    progs <- forM games \Game{..} -> do
        name <- labelled $ db $ (<> [0]) . take 16 . map (fromIntegral . ord . toUpper) $ gameTitle
        joy <- labelled $ let (u, d, l, r, f) = gameJoy in db [u, d, l, r, f]
        prog <- labelled $ db $ encodeQuirks gameQuirks <> gameImage
        pure (name, joy, prog)
    pure ()
