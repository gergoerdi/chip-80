{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Target.HomeLab.HTP where

import Z80
import Z80.Utils

import qualified Data.ByteString as BS

htp :: BS.ByteString -> ASMBlock -> BS.ByteString
htp label mainBlock = mconcat
    [ leader
    , record label $ org 0x4002 $ dw [asmOrg mainBlock]
    , BS.singleton 0x01
    , leader
    , record mempty mainBlock
    , BS.singleton 0x00
    ]
  where
    leader = BS.replicate 100 0x00

    record label block = mconcat
        [ BS.singleton 0xa5
        , label
        , BS.singleton 0x00
        , word $ asmOrg block
        , word . fromIntegral $ BS.length bs
        , bs
        , crc bs
        ]
      where
        bs = asmData block

    crc = BS.singleton . BS.foldr' (+) 0

    word w = BS.pack [lo, hi]
      where
        (lo, hi) = wordBytes w
