module Main where

import Parser
import Lexer
import TypeCheck
import AST
import MiddleCode

main :: IO()
main = do 
    content <- getContents
    let ast = parse $ getTokens content
    print ast
    if (checkProgram ast)
        then codeGeneration ast
        else putStrLn "typecheck error"

codeGeneration :: AST -> IO()
codeGeneration ast = do
                    let middlecode = transProgram ast
                    print middlecode
                    
                    