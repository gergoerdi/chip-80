{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Target.GameBoy.Operations where

import Z80

import qualified Data.ByteString as BS
import System.Directory
import Data.Word

class LoadH tgt src where
    ldh :: tgt -> src -> Z80ASM

instance (addr ~ Word8) => LoadH A [addr] where
    ldh A [addr] = code [0xf0, addr]

instance (addr ~ Word8) => LoadH [addr] A where
    ldh [addr] A = code [0xe0, addr]

ldhVia :: (LoadH tgt tmp, Load tmp src) => tmp -> tgt -> src -> Z80ASM
ldhVia tmp tgt src = do
    ld tmp src
    ldh tgt tmp
