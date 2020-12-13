module Main where

import MachineSymTest
import System.Exit

main :: IO ()
main = do test <- runMachineSymTest
          if test then exitSuccess else exitFailure
