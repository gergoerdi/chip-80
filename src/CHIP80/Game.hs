module CHIP80.Game (Game(..), readGames) where

import CHIP80.Quirks

import Z80
import Z80.ZX0.Compress
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

data Game = Game
    { gameTitle :: String
    , gameQuirks :: Quirks Bool
    , gameImage :: BS.ByteString
    }

readGames :: [Key] -> FilePath -> IO [Game]
readGames selected yamlPath = do
    let dir = takeDirectory yamlPath
    yaml <- decodeFileThrow yamlPath

    games <- forM (filter ((`elem` selected) . fst) $ KeyMap.toList yaml) \(name, vals) -> do
        let fileName = dir </> toString name <.> "ch8"
            stringValue (String s) = unpack s
            gameTitle = maybe (takeBaseName fileName) stringValue $
                KeyMap.lookup (fromString "title") vals
            gameQuirks = applyDefaults $ case fromJSON (Object vals) of
                Error err -> error err
                Success x -> x
        gameImage <- compressForward =<< BS.readFile fileName
        pure Game{..}

    sizes <- forM games \Game{..} -> do
        let size = BS.length gameImage
        printf "%-16s %4d\n" gameTitle size
        pure size
    printf "%-16s %d\n" "Total:" (sum sizes)

    pure games
