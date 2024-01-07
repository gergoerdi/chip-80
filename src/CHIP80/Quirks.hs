{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module CHIP80.Quirks where

import Data.Maybe (fromMaybe)
import Data.Yaml
import GHC.Generics

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
