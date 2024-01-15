module Target.TVC where

import Target.TVC.Shell
import Target.TVC.Cas

import Z80

import qualified Data.ByteString as BS
import Data.String (fromString)
import System.FilePath
import System.Directory

emit :: IO ()
emit = do
    prog <- org 0x1a00 <$> withGamesFrom "data/games"
    let name = "_build/chip80-tvc"

    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData prog
    BS.writeFile (name <.> "cas") $ cas prog
