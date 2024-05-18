module Target.HomeLab.HL4 where

import Target.HomeLab.HL4.Shell
import Target.HomeLab.HL4.HTP
import qualified Z80.Machine.HomeLab.HTP as HTP

import Z80

import qualified Data.ByteString as BS
import Data.String (fromString)
import System.FilePath
import System.Directory

emit :: IO ()
emit = do
    prog <- org 16700 <$> withGamesFrom "data/games"
    let name = "_build/chip80-hl4"

    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData prog
    let htp = htpWithAutoStart (fromString $ takeBaseName name) prog
    BS.writeFile (name <.> "htp") htp
    HTP.renderToWav (name <.> "wav") htp
