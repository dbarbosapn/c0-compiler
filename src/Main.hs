module Main where

import Parser
import Lexer
import TypeCheck

main :: IO()
main = do 
    content <- getContents
    let ast = parse $ getTokens content
    if (checkProgram ast)
        then print ast
        else putStrLn "typecheck error"

