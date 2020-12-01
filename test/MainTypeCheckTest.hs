module Main where

import TypeCheckTest
import System.Exit

main :: IO ()
main = do test <- runTypeCheckTest
          if test then exitSuccess else exitFailure
