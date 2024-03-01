module Target.TRS80 where

import Z80
import Z80.Utils
import Z80.Machine.TRS80.Defs
import Z80.Machine.TRS80.Cas
import Target.TRS80.Video2x3

import Data.Word
import qualified Data.ByteString as BS
import Data.String (fromString)
import System.FilePath
import System.Directory

emit :: IO ()
emit = do
    logo <- BS.readFile ("data/games" </> "logo-url.png")

    let prog = org 0x4300 mdo
            di

            ld DE $ videoStart +
              rowstride * ((numLines - fromIntegral logoHeight) `div` 2) +
              (rowstride - fromIntegral logoWidth) `div` 2
            ld HL logoData
            decLoopB logoHeight do
                push BC
                ld BC $ fromIntegral logoWidth
                ldir
                ld BC $ fromIntegral $ rowstride - logoWidth
                ex DE HL
                add HL BC
                ex DE HL
                pop BC

            ld HL videoStart
            ld A 0x00
            loopForever do
                ld [HL] A
                inc A

            let (logoWidth, logoHeight, logoBytes) = encodeFromPng blockChars logo
            logoData <- labelled $ db logoBytes
            pure ()

    let name = "_build/chip80-trs80"

    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData prog
    BS.writeFile (name <.> "cas") $ cas (fromString "CHIP80") prog

blockChars :: [Word8]
blockChars = [ 0x80.. 0xbf ]
