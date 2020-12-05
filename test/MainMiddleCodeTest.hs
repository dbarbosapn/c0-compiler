module Main where

import MiddleCodeTest
import System.Exit

main :: IO ()
main = do test <- runMiddleCodeTest
          if test then exitSuccess else exitFailure
