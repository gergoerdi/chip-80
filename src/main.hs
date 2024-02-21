module Main where

import qualified Target.HomeLab.HL2 as HL2
import qualified Target.HomeLab.HL4 as HL4
import qualified Target.TVC as TVC
import qualified Target.GameBoy as GameBoy

main :: IO ()
main = sequence_
    [ HL2.emit
    , HL4.emit
    , TVC.emit
    , GameBoy.emit
    ]
