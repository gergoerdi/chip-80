module Target.HomeLab.HL2 where

import Target.HomeLab.HL2.Shell
import Target.HomeLab.HTP

import Z80

import qualified Data.ByteString as BS
import Data.String (fromString)
import System.FilePath
import System.Directory

main :: IO ()
main = do
    emit "_build/chip80" =<< org 16700 <$> withGamesFrom "data/games"

emit :: String -> ASMBlock -> IO ()
emit name block = do
    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData block
    BS.writeFile (name <.> "htp") $ htp (fromString $ takeBaseName name) block
