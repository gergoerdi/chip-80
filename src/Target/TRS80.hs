module Target.TRS80 where

import Z80
import Z80.Utils
import Z80.Machine.TRS80.Defs
import Z80.Machine.TRS80.Cas

import Target.TRS80.Video2x3
import Target.TRS80.Input

import CHIP80.Video

import Data.Word
import qualified Data.ByteString as BS
import Data.String (fromString)
import System.FilePath
import System.Directory
import Data.Bits
import Control.Monad

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

            -- Wait for Break key
            withLabel \loop -> do
                call scanKeys
                jp NZ loop

            drawScreen vidBuf

            -- Debug heartbeat
            ld HL videoStart
            loopForever do
                ld [HL] A
                inc A

            scanKeys <- labelled $ scanKeys_ keyBuf

            let (logoWidth, logoHeight, logoBytes) = encodeFromPng blockChars logo
            logoData <- labelled $ db logoBytes

            vidBuf <- labelled $ db testPicture
            keyBuf <- labelled $ db $ replicate 16 0x00
            pure ()

    let name = "_build/chip80-trs80"

    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData prog
    BS.writeFile (name <.> "cas") $ cas (fromString "CHIP80") prog

blockChars :: [Word8]
blockChars = [ 0x80.. 0xbf ]

testPicture :: [Word8]
testPicture = concatMap splitUp $
    [ 0x00_00_00_00_00_00_00_00
    , 0x00_0f_a0_00_01_00_30_00
    , 0x00_02_0d_19_c7_49_90_00
    , 0x00_02_2a_a5_29_4a_00_00
    , 0x00_02_28_bd_29_49_00_00
    , 0x00_02_28_a1_29_48_80_00
    , 0x00_02_28_9d_27_3b_00_00
    , 0x00_00_00_00_00_00_00_00
    , 0x00_00_00_00_00_00_00_00
    , 0x00_1f_18_0c_f8_00_fe_00
    , 0x00_3f_b8_1d_fc_01_c7_00
    , 0x00_71_b8_1d_ce_03_83_00
    , 0x00_e0_38_01_c6_03_83_00
    , 0x00_e5_38_0d_c6_03_83_00
    , 0x00_e0_3f_1d_c6_01_c6_00
    , 0x00_e8_bf_9d_c6_f0_fc_00
    , 0x00_e7_39_dd_ce_f1_ce_00
    , 0x00_e0_38_dd_fc_03_87_00
    , 0x00_e0_38_dd_f8_07_03_00
    , 0x00_e0_38_dd_c0_07_03_00
    , 0x00_e0_38_dd_d4_27_03_00
    , 0x00_71_b8_dd_dc_67_87_00
    , 0x00_3f_b8_dd_c4_23_fe_00
    , 0x00_1f_38_dd_c5_71_fc_00
    , 0x00_00_00_00_00_00_00_00
    , 0x00_00_00_00_00_00_00_00
    , 0x00_07_31_a0_30_28_60_00
    , 0x00_02_4a_38_44_8e_90_00
    , 0x00_02_79_20_24_a8_f0_00
    , 0x00_02_40_a0_14_a8_80_00
    , 0x00_02_3b_18_63_a6_70_00
    , 0x00_00_00_00_00_00_00_00
    ]
  where
    splitUp :: Word64 -> [Word8]
    splitUp x = [ fromIntegral $ x `shiftR` i | i <- [56, 48 .. 0] ]
