module Main where

import Lexer
import TestLexer
import System.Exit

main :: IO()
main = do runTests
          return ()
