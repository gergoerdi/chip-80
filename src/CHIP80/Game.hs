module CHIP80.Game (readGames) where

import CHIP80.Quirks
import ZX0.Compress

import Z80
import Control.Monad
import qualified Data.ByteString as BS
import Data.String (fromString)

import Data.Yaml
import Data.Aeson (fromJSON, Result(..), Object)
import Data.Aeson.KeyMap as KeyMap (KeyMap, toList, lookup)
import Data.Aeson.Key (Key, toString)
import Data.Text (unpack)
import System.FilePath
import Text.Printf

readGames :: [Key] -> FilePath -> IO [(String, Quirks Bool, BS.ByteString)]
readGames selected yamlPath = do
    let dir = takeDirectory yamlPath
    yaml <- decodeFileThrow yamlPath

    let games =
            [ (title, quirks, fileName)
            | (name, vals) <- KeyMap.toList yaml
            , name `elem` (selected :: [Key])
            , let fileName = dir </> toString name <.> "ch8"
                  stringValue (String s) = unpack s
                  title = maybe (takeBaseName fileName) stringValue $
                      KeyMap.lookup (fromString "title") vals
                  quirks = applyDefaults $ case fromJSON (Object vals) of
                      Error err -> error err
                      Success x -> x
            ]

    images <- forM games \(title, quirks, fileName) -> do
        (image, _) <- compressForward =<< BS.readFile fileName
        pure (title, quirks, image)

    sizes <- forM images \(title, _quirks, image) -> do
        let size = BS.length image
        printf "%-16s %4d\n" title size
        pure size
    printf "%-16s %d\n" "Total:" (sum sizes)

    pure images
