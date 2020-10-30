 -- What is between { } is actual haskell code
{
module Lexer where

import Data.Char
}

%wrapper "basic"

$digit = [0-9] -- digits
$alpha = [_a-zA-Z] -- alphabetc characters
$white = [\ \n\r\t] -- Space chars

tokens :-

$white+ ; -- Ignore white characters like ' ', '\t' etc
"//".* ; -- Ignore line comments. '.' refers to anything exept '\n'
"/*"[.\n]*"*/" ; -- Ignore multiple lines of comments

-- Special characters like { , ) etc
"{" { \_ -> LBRACE }
"}" { \_ -> RBRACE }
")" { \_ -> RPAREN }
"(" { \_ -> LPAREN }

-- Float and Int numbers
$digit+                                           { \s -> INT (read s :: Int) }

-- Bool
"true"    { \s -> BOOL True }
"false"   { \s -> BOOL False }

-- Operators
"+"       { \_ -> ADD }
"-"       { \_ -> SUB }
"/"       { \_ -> DIV }
"*"       { \_ -> MULT }

{
data Token = LPAREN
     | RPAREN
     | LBRACE
     | RBRACE
     | INT Int
     | BOOL Bool
     | ADD
     | SUB
     | DIV
     | MULT
     deriving (Eq, Show)

getTokens :: String -> [Token]
getTokens str = alexScanTokens str

}
