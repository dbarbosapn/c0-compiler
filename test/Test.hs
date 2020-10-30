module Main where

import Lexer
import LexerTest
import System.Exit

main :: IO()
main = do 
        passed <- runLexerTests
        if passed then exitSuccess else exitFailure
