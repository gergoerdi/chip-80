module CHIP80.Game (Game(..), readGames) where

import CHIP80.Quirks

import Z80
import Z80.ZX0.Compress
import Control.Monad
import Data.Word
import qualified Data.ByteString as BS
import Data.String (fromString)

import Data.Yaml
import Data.Aeson (fromJSON, Result(..), Object)
import Data.Aeson.KeyMap as KeyMap (KeyMap, toList, lookup)
import Data.Aeson.Key (Key, toString)
import Data.Text (unpack)
import System.FilePath
import Text.Printf
import Numeric (readHex)

data Game = Game
    { gameTitle :: String
    , gameJoy :: (Word8, Word8, Word8, Word8, Word8) -- Up, down, left, right, fire
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
            gameJoy = case stringValue <$> KeyMap.lookup (fromString "joy") vals of
                Just [u, d, l, r, f] -> (readJoy u, readJoy d, readJoy l, readJoy r, readJoy f)
                  where
                    readJoy c = case readHex [c] of
                        [(x, "")] -> x
                        _ -> error $ printf "readJoy: '%c'" c
                _ -> (0x0, 0x0, 0x0, 0x0, 0x0)
        gameImage <- compressForward =<< BS.readFile fileName
        pure Game{..}

    sizes <- forM games \Game{..} -> do
        let size = BS.length gameImage
        printf "%-16s %4d\n" gameTitle size
        pure size
    printf "%-16s %d\n" "Total:" (sum sizes)

    pure games
