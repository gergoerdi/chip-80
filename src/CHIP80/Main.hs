{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module CHIP80.Main (game) where

import HL2
import CHIP80.HL2.Machine
import Z80.ZX0

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
import Data.Default

game :: BS.ByteString -> Z80ASM
game image = mdo
    let baseAddr = 0x7000
    ld SP $ baseAddr - (256 + 16) - 1

    ld IX compressedProg
    machine baseAddr
    compressedProg <- labelled $ db image
    pure ()
