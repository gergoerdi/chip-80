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
    let baseAddr = 0x7000
    ld SP $ baseAddr - (256 + 16) - 1

    let (quirks, prog) = progs!!4
    ld IX quirks
    ld IY prog
    machine baseAddr

    progs <- forM images \(name, Quirks{..}, image) -> do
        let boolToByte = \case
                True -> 1
                False -> 0

        quirks <- labelled $ db . map boolToByte $
            [ shiftVY, resetVF, incrementPtr, videoWait, clipSprites ]
        prog <- labelled $ db image
        pure (quirks, prog)
    pure ()
