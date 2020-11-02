module Main where

import Parser
import Lexer

main :: IO()
main = do  content <- getContents
           print $ parse $ getTokens content
