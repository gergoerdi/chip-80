{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module CHIP80.Quirks where

import Data.Maybe (fromMaybe)
import Data.Yaml
import GHC.Generics
import qualified Data.ByteString as BS

data Quirks a = Quirks
    { shiftVY, resetVF, incrementPtr, videoWait, clipSprites :: a
    }
    deriving (Generic, Show)

instance FromJSON (Quirks (Maybe Bool))

applyDefaults :: Quirks (Maybe Bool) -> Quirks Bool
applyDefaults Quirks{..} = Quirks
    { shiftVY = fromMaybe True shiftVY
    , resetVF = fromMaybe True resetVF
    , incrementPtr = fromMaybe True incrementPtr
    , videoWait = fromMaybe True videoWait
    , clipSprites = fromMaybe True clipSprites
    }

encodeQuirks :: Quirks Bool -> BS.ByteString
encodeQuirks Quirks{..} = BS.pack . map encodeBool $
    [ shiftVY, resetVF, incrementPtr, videoWait, clipSprites ]
  where
    encodeBool True  = 0x01
    encodeBool False = 0x00
