module Main where

import ParserTest
import System.Exit

main :: IO()
main = do test <- runParserTests
          if test then exitSuccess else exitFailure
