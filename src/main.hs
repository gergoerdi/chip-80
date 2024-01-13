module Main where

import qualified Target.HomeLab.HL2 as HL2
import qualified Target.TVC as TVC

main :: IO ()
main = do
    HL2.emit
    TVC.emit
