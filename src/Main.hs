module Main where

import Parser
import Lexer
import TypeCheck
import AST
import MiddleCode
import MachineSym

main :: IO()
main = do 
    content <- getContents
    let tokens = getTokens content
    putStrLn "Tokens:"
    print tokens
    let ast = parse tokens
    putStrLn "\nAST:"
    print ast
    if (checkProgram ast)
        then codeGeneration ast
        else putStrLn "\nTypeCheck: Error!"

codeGeneration :: AST -> IO()
codeGeneration ast = do
                    putStrLn "\nTypeCheck: OK!"
                    let middlecode = transProgram ast
                    putStrLn "\nMiddle Code:"
                    print middlecode
                    putStrLn "\nMachine Code:"
                    let mips = generateMips middlecode
                    putStrLn mips