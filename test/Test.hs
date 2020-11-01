module Main where

import Lexer
import LexerTest
import ParserTest
import System.Exit

main :: IO()
main = do 
        passedLexer <- runLexerTests
        passedParser <- runParserTests
        if passedLexer && passedParser then exitSuccess else exitFailure
