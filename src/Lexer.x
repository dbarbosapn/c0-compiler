 -- What is between { } is actual haskell code
{
module Lexer where

import Data.Char
}

%wrapper "basic"

-- Macros
$digit = [0-9] -- Digits
$alpha = [_a-zA-Z] -- alphabetc characters
$alphaDigit = [_a-zA-Z0-9]
-- Alex already has White chars macro called $white

-- Its not necessary to have the following macros but it makes it
-- easier to read
$strDel = [\"] --"
$charDel = [\'] --'

tokens :-

$white+ ; -- Ignore white characters like ' ', '\t' etc
"//".* ; -- Ignore line comments. '.' refers to anything exept '\n'
"/*"[.\n]*"*/" ; -- Ignore multiple lines of comments

-- Separators
"{" { \_ -> LBRACE }
"}" { \_ -> RBRACE }
")" { \_ -> RPAREN }
"(" { \_ -> LPAREN }
";" { \_ -> SEMICOLON }
"[" { \_ -> LBRACKET }
"]" { \_ -> RBRACKET }
"," { \_ -> COMMA }


-- Numeric Constants
("0" | [1-9]$digit+)     { \s -> INT (read s :: Int) }
"0"[xX][0-9a-fA-F]+      { \s -> INT $ hexToInt s }

-- Bool constants
"true"    { \s -> BOOL True }
"false"   { \s -> BOOL False }

-- Char Constant
$charDel($printable|$white)$charDel { \s -> CHAR $ head $ tail s }

-- String Constant
$strDel($printable|$white)*$strDel { \s -> STRING [ x | x <- s, x /= '\"' ] }

-- Arithmetic Operators
"+" { \_ -> A_OP ADD }
"-" { \_ -> A_OP SUB }
"/" { \_ -> A_OP DIV }
"*" { \_ -> A_OP MULT }
"%" { \_ -> A_OP MOD }

-- Assignment
"=" { \_ -> ASSIGN }

-- Relational Operators
"==" { \_ -> R_OP EQUAL }
"<=" { \_ -> R_OP LESS_OR_EQUAL }
">=" { \_ -> R_OP MORE_OR_EQUAL }
"!=" { \_ -> R_OP NOT_EQUAL }
"<"  { \_ -> R_OP LESS }
">"  { \_ -> R_OP MORE }

-- Reserved Words
"for"     { \_ -> FOR }
"while"   { \_ -> WHILE }
"if"      { \_ -> IF }
"then"    { \_ -> THEN }
"else"    { \_ -> ELSE }
"return"  { \_ -> RETURN }

-- Types
"int"          { \_ -> T_INT }
"char"         { \_ -> T_CHAR }
"bool"         { \_ -> T_BOOL }

-- ID
$alpha$alphaDigit* { \s -> ID s }

{
-- Arithmetic Operator
data A_Operator = ADD | SUB | DIV | MULT | MOD deriving (Eq, Show)

-- Relational Operator
data R_Operator = EQUAL | LESS_OR_EQUAL | MORE_OR_EQUAL | NOT_EQUAL | LESS | MORE deriving (Eq, Show)

data Token = 
     -- Separators
     LPAREN
     | RPAREN
     | LBRACE
     | RBRACE
     | RBRACKET
     | LBRACKET
     | SEMICOLON
     | COMMA
     -- Constants
     | INT Int
     | BOOL Bool
     | STRING String
     | CHAR Char
     -- Operators
     | A_OP A_Operator
     | R_OP R_Operator
     | ASSIGN
     -- Reserved words
     | FOR
     | WHILE
     | IF
     | THEN
     | ELSE
     | RETURN
     -- Types
     | T_INT
     | T_CHAR
     | T_BOOL
     -- IDs (Function names, variables...)
     | ID String
     deriving (Eq, Show)

hexToInt :: String -> Int
hexToInt (_:_:hex) = sum [ y * 16^x  | (x, y) <- zip [0..] $ map digitToInt $ reverse hex ]

getTokens :: String -> [Token]
getTokens str = alexScanTokens str

}
