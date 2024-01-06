{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import qualified CHIP80.Main as CHIP80

import Z80
import Z80.Utils
import Z80.ZX0.Compress

import Data.Word
import qualified Data.ByteString as BS
import Control.Monad
import Data.Bits
import Data.Char
import Text.Printf
import Data.String (fromString)
import System.FilePath
import System.Directory

main :: IO ()
main = do
do
    let files =
            [ "/home/cactus/prog/rust/chirp8-sdl/hidden.ch8"
            -- , "/home/cactus/prog/haskell/chip8/import/CHIP8/GAMES/Rush Hour.c8"
            -- , "/home/cactus/prog/rust/chirp8-sdl/hello.ch8"
            -- , "/home/cactus/prog/haskell/chip8/import/CHIP8/GAMES/TICTAC"
            -- , "/home/cactus/prog/rust/chirp8-sdl/test-roms/1-chip8-logo.ch8"
            -- , "/home/cactus/prog/rust/chirp8-sdl/test-roms/2-ibm-logo.ch8"
            -- , "/home/cactus/prog/rust/chirp8-sdl/test-roms/3-corax+.ch8"
            -- , "/home/cactus/prog/rust/chirp8-sdl/test-roms/4-flags.ch8"
            -- , "/home/cactus/prog/rust/chirp8-sdl/test-roms/5-quirks.ch8"
            , "/home/cactus/prog/rust/chirp8-sdl/test-roms/6-keypad.ch8"
            ]
    images <- forM files \file -> do
        let name = take 16 . map toUpper $ takeBaseName file
        (image, _) <- compressForward =<< BS.readFile file
        pure (name, image)

    sizes <- forM images \(name, image) -> do
        let size = BS.length image
        printf "%-16s %4d\n" name size
        pure size
    printf "%-16s %d\n" "Total:" (sum sizes)

    emit "_build/chip80" $ org 20000 $ CHIP80.game images

emit :: String -> ASMBlock -> IO ()
emit name block = do
    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData block
    BS.writeFile (name <.> "htp") $ htp (fromString $ takeBaseName name) block

htp :: BS.ByteString -> ASMBlock -> BS.ByteString
htp label mainBlock = mconcat
    [ leader
    , record label $ org 0x4002 do
            dw [asmOrg mainBlock]
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
