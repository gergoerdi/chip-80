module Target.HomeLab.HL4.Shell (withGamesFrom) where

import Target.HomeLab.HL4.Defs
import CHIP80.Quirks
import CHIP80.Game
import Target.HomeLab.HL4.Machine
import Target.HomeLab.HL4.Video64Small (encodeFromPng)
import ZX0
import ZX0.Compress

import Z80
import Z80.Utils
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
    selected <- decodeFileThrow (dir </> "hl2.yaml")
    images <- readGames selected (dir </> "games.yaml")
    pure $ game images logo

game :: [(String, Quirks Bool, BS.ByteString)] -> BS.ByteString -> Z80ASM
game images logo = mdo
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

        ld DE $ videoStart + rowstride * 2 + (rowstride - fromIntegral logoWidth) `div` 2 - 3
        ld HL progCopy
        skippable \end -> loopForever do
            ld A [HL]
            inc HL
            Z80.and A
            jp Z end
            ld [DE] A
            inc DE

        ld DE $ videoStart + rowstride * (2 + fromIntegral logoHeight + 2) + 38
        ld HL logoCopy
        skippable \end -> loopForever do
            ld A [HL]
            inc HL
            Z80.and A
            jp Z end
            ld [DE] A
            inc DE

        ld DE $ videoStart + rowstride * 4 + (rowstride - fromIntegral logoWidth) `div` 2
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

        let cr = do
                ld A $ fromIntegral . ord $ '\r'
                printA

        decLoopB 20 do
            cr

        -- Print menu of available programs
        let space = do
                ld A 0x20
                printA
        ld C $ fromIntegral . ord $ '0'
        ld IX titleTable
        decLoopB (fromIntegral $ length progs) do
            ld L [IX]
            inc IX
            ld H [IX]
            inc IX

            space
            ld A C
            inc C
            printA
            space

            call println

        withLabel \inputLoop -> do
            getKeyA
            sub $ fromIntegral . ord $ '0'
            jp C inputLoop

            cp (fromIntegral $ length progs + 1)
            jp NC inputLoop

            ld D 0
            sla A
            ld E A

            ld HL progTable
            add HL DE
            ld C [HL]
            inc HL
            ld B [HL]
            push BC
            pop IX

            call machine

    print <- labelled do
        loopForever do
            ld A [HL]
            Z80.and A
            ret Z
            printA
            inc HL

    println <- labelled do
        call print
        ld A $ fromIntegral . ord $ '\r'
        jp 0x0284

    let (logoWidth, logoHeight, logoBytes) = encodeFromPng logo
    logoData <- labelled $ db logoBytes
    progCopy <- labelled $ db $ (++ [0]) $ map (fromIntegral . ord . toUpper) $
        "Gergo Erdi's"
    logoCopy <- labelled $ db $ (++ [0]) $ map (fromIntegral . ord . toUpper) $
        "Logo by Tim Franssen"

    machine <- labelled $ machine_ baseAddr

    titleTable <- labelled $ dw [ title | (title, _) <- progs ]
    progTable <- labelled $ dw [ prog | (_, prog) <- progs ]

    progs <- forM images \(title, quirks, image) -> do
        let boolToByte = \case
                True -> 1
                False -> 0

        name <- labelled $ db $ (<> [0]) . take 16 . map (fromIntegral . ord . toUpper) $ title
        prog <- labelled $ db $ encodeQuirks quirks <> image
        pure (name, prog)


    pure ()
