module Main where

import LexerTest
import System.Exit

main :: IO()
main = do test <- runLexerTests
          if test then exitSuccess else exitFailure
