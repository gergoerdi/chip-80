{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards, NamedFieldPuns, LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module CHIP80.Main (withGamesFrom) where

import HL2
import CHIP80.Quirks
import CHIP80.HL2.Machine
import Z80.ZX0
import Z80.ZX0.Compress

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
    yaml <- decodeFileThrow (dir </> "games.yaml")

    images <- forM (KeyMap.toList yaml) \(name, vals) -> do
        let fileName = dir </> toString name <.> "ch8"
        (image, _) <- compressForward =<< BS.readFile fileName

        let stringValue (String s) = unpack s
            title = maybe (takeBaseName fileName) stringValue $
                KeyMap.lookup (fromString "title") vals
            quirks = applyDefaults $ case fromJSON (Object vals) of
                Error err -> error err
                Success x -> x
        pure (title, quirks, image)

    sizes <- forM images \(title, _quirks, image) -> do
        let size = BS.length image
        printf "%-16s %4d\n" title size
        pure size
    printf "%-16s %d\n" "Total:" (sum sizes)

    pure $ game images

game :: [(String, Quirks Bool, BS.ByteString)] -> Z80ASM
game images = mdo
    -- Restore input vector
    ldVia A [0x4002] 0x06
    ldVia A [0x4003] 0x03

    let baseAddr = 0x7000
    ld SP $ baseAddr - (256 + 16) - 1

    loopForever do
        -- Clear screen
        ld A 0x0c
        rst 0x28

        ld HL banner
        call println

        -- Print menu of available programs
        let space = do
                ld A 0x20
                rst 0x28
        ld C $ fromIntegral . ord $ '1'
        ld IX titleTable
        decLoopB (fromIntegral $ length progs) do
            ld L [IX]
            inc IX
            ld H [IX]
            inc IX

            space
            ld A C
            inc C
            rst 0x28
            space

            call println

        withLabel \inputLoop -> do
            rst 0x18
            sub $ fromIntegral . ord $ '1'
            jp C inputLoop

            cp (fromIntegral $ length progs + 1)
            jp NC inputLoop

            ld D 0
            sla A
            ld E A

            ld HL quirksTable
            add HL DE
            ld C [HL]
            inc HL
            ld B [HL]
            push BC
            pop IX

            ld HL progTable
            add HL DE
            ld C [HL]
            inc HL
            ld B [HL]
            push BC
            pop IY

            call machine

    -- TODO: share this with rest of the code
    banner <- labelled $ db $ (++ [0]) $ map (fromIntegral . ord . toUpper) $ invert "   CHIP-80     https://gergo.erdi.hu/   "

    print <- labelled do
        loopForever do
            ld A [HL]
            Z80.and A
            ret Z
            rst 0x28
            inc HL

    println <- labelled do
        call print
        ld A $ fromIntegral . ord $ '\r'
        rst 0x28
        ret

    machine <- labelled $ machine_ baseAddr

    progs <- forM images \(title, Quirks{..}, image) -> do
        let boolToByte = \case
                True -> 1
                False -> 0

        name <- labelled $ db $ (<> [0]) . take 16 . map (fromIntegral . ord . toUpper) $ title
        quirks <- labelled $ db . map boolToByte $
            [ shiftVY, resetVF, incrementPtr, videoWait, clipSprites ]
        prog <- labelled $ db image
        pure (name, quirks, prog)

    titleTable <- labelled $ dw [ title | (title, _, _) <- progs ]
    quirksTable <- labelled $ dw [ quirks | (_, quirks, _) <- progs ]
    progTable <- labelled $ dw [ prog | (_, _, prog) <- progs ]
    pure ()
