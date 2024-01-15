module Target.HomeLab.HL2 where

import Target.HomeLab.HL2.Shell
import Target.HomeLab.HL2.HTP

import Z80

import qualified Data.ByteString as BS
import Data.String (fromString)
import System.FilePath
import System.Directory

emit :: IO ()
emit = do
    prog <- org 16700 <$> withGamesFrom "data/games"
    let name = "_build/chip80-hl2"

    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData prog
    BS.writeFile (name <.> "htp") $ htpWithAutoStart (fromString $ takeBaseName name) prog
