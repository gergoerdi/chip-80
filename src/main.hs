module Main where

import qualified Target.HomeLab.HL2 as HL2
import qualified Target.HomeLab.HL4 as HL4
import qualified Target.TVC as TVC
import qualified Target.TRS80 as TRS80

main :: IO ()
main = sequence_
    [ TRS80.emit
    , HL2.emit
    , HL4.emit
    , TVC.emit
    ]
