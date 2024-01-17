module Target.HomeLab.HL4.Shell (withGamesFrom) where

import Target.HomeLab.HL4.Defs
import CHIP80.Quirks
import CHIP80.Game
import Target.HomeLab.HL4.Machine
import Target.HomeLab.HL4.Video64 (encodeFromPng)
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
    selected <- decodeFileThrow (dir </> "hl4.yaml")
    images <- readGames selected (dir </> "games.yaml")
    pure $ game images logo

game :: [(String, Quirks Bool, BS.ByteString)] -> BS.ByteString -> Z80ASM
game images logo = mdo
    -- pageVideo

    -- ld DE videoStart
    -- ld HL banner
    -- skippable \end -> loopForever do
    --     ld A [HL]
    --     inc HL
    --     Z80.and A
    --     jp Z end
    --     ld [DE] A
    --     inc DE

    -- ld DE $ videoStart + rowstride * 4
    -- ld HL logoData
    -- decLoopB logoHeight do
    --     push BC
    --     ld BC $ fromIntegral logoWidth
    --     ldir
    --     ld BC $ fromIntegral $ rowstride - logoWidth
    --     ex DE HL
    --     add HL BC
    --     ex DE HL
    --     pop BC

    -- pageRAM

    loopForever do
        ld IX quirks
        ld IY image
        call machine

    banner <- labelled $ db $ (++ [0]) $ map (fromIntegral . ord . toUpper) $ "CHIP-80   https://gergo.erdi.hu/"

    let (logoWidth, logoHeight, logoBytes) = encodeFromPng logo
    logoData <- labelled $ db logoBytes

    let baseAddr = 0x7000
    machine <- labelled $ machine_ baseAddr

    image <- labelled $ db $ let [(_, _, image)] = images in image
    quirks <- labelled $ db $ let [(_, quirks, _)] = images in encodeQuirks quirks
    pure ()
