module Target.HomeLab.HL4.Shell ({- withGamesFrom -} game) where

import Target.HomeLab.HL4.Defs
import CHIP80.Quirks
-- import Target.HomeLab.HL2.Machine
-- import Target.HomeLab.HL2.Video (encodeFromPng)
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

game :: [(String, Quirks Bool, BS.ByteString)] -> BS.ByteString -> Z80ASM
game images logo = mdo
    pageVideo
    ld DE videoStart
    ld HL banner
    skippable \end -> loopForever do
        ld A [HL]
        inc HL
        Z80.and A
        jp Z end
        ld [DE] A
        inc DE
        inc DE
    pageRAM
    loopForever $ pure ()

    banner <- labelled $ db $ (++ [0]) $ map (fromIntegral . ord . toUpper) $ "CHIP-80   https://gergo.erdi.hu/"

    -- let (logoWidth, logoHeight, logoBytes) = encodeFromPng logo
    -- logoData <- labelled $ db logoBytes
    pure ()
