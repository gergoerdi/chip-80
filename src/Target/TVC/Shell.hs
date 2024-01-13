module Target.TVC.Shell (withGamesFrom) where

import CHIP80.Quirks
import Target.TVC.Machine
import Target.TVC.Defs
import Target.TVC.Video
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
    logo <- BS.readFile (dir </> "logo.png")
    let (logoWidth, logoHeight, logoBytes) = encodeFromPng logo

    let file = "1-chip-logo"
    let file = "2-ibm-logo"
    let file = "hidden"
    (image, _) <- compressForward =<< BS.readFile (dir </> file <.> "ch8")
    -- let quirks = Quirks{ shiftVY = False, resetVF = False, incrementPtr = False, videoWait = False, clipSprites = False }

    pure $ mdo
        ld SP 0x16ad

        setVideoMode

        pageVideo
        ld HL logo
        ld DE videoStart
        decLoopB logoHeight do
            push BC
            replicateM_ 3 do
                push HL
                ld BC 64
                ldir
                pop HL
            ld BC 64
            ldir
            pop BC
        pageRAM

        ld IX quirks
        ld IY prog
        call machine

        loopForever $ pure ()

        logo <- labelled $ db logoBytes
        prog <- labelled $ db image
        quirks <- labelled $ db [0, 0, 0, 0, 0]
        machine <- labelled $ machine_ 0xe000
        pure ()
