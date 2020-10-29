module Main where

import Lexic.TestLexer
import System.Exit

main :: IO ()
main = do putStrLn "Hello, Haskell!"
          passedLexic <- runTests
          if passedLexic then exitSuccess else exitFailure
