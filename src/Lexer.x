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


-- For some reason when i try to "track" '%' char for the mod the whole
-- program breaks. To solve this i will just define a macro. If any other
-- char breaks like this, just put it here and we will solve it later

-- Its not necessary to have the following macros but it makes it
-- easier to read
$strDel = [\"] --"
$charDel = [\'] --'

tokens :-

$white+ ; -- Ignore white characters like ' ', '\t' etc
"//".* ; -- Ignore line comments. '.' refers to anything exept '\n'
"/*"[.\n]*"*/" ; -- Ignore multiple lines of comments

-- Special characters like { , ) etc
"{" { \_ -> LBRACE }
"}" { \_ -> RBRACE }
")" { \_ -> RPAREN }
"(" { \_ -> LPAREN }
";" { \_ -> SEMICOLON }
"[" { \_ -> LBRACKET }
"]" { \_ -> RBRACKET }


-- Int Numbers
[1-9]$digit+ { \s -> INT (read s :: Int) }

-- Hex Numbers
"0"[xX][0-9a-fA-F]+ { \s -> INT $ hexToInt s }

-- Bool
"true"    { \s -> BOOL True }
"false"   { \s -> BOOL False }

-- Operators
"+" { \_ -> ADD }
"-" { \_ -> SUB }
"/" { \_ -> DIV }
"*" { \_ -> MULT }

-- Comparisons
"=" { \_ -> EQUAL}
"==" { \_ -> IS_EQUAL }
"<=" { \_ -> IS_LESS_OR_EQUAL }
">=" { \_ -> IS_MORE_OR_EQUAL }
"/=" { \_ -> IS_NOT_EQUAL }
"<" { \_ -> IS_LESS }
">" { \_ -> IS_MORE }

-- Reserved Words
"for" { \_ -> FOR }
"while" { \_ -> WHILE }
"if" { \_ -> IF }
"then" { \_ -> THEN }
"else" { \_ -> ELSE }

-- ID
$alpha$alphaDigit* { \s -> ID s }

-- Char
$charDel($printable|$white)$charDel { \s -> CHAR $ head $ tail s }

-- String
$strDel($printable|$white)*$strDel { \s -> STRING [ x | x <- s, x /= '\"' ] }

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
     | MOD
     | STRING String
     | CHAR Char
     | RBRACKET
     | LBRACKET
     | SEMICOLON
     | EQUAL
     -- The following Tokens NEED to have _ in order to not have clashes with
     -- prelude functions. Weird Stuff but ok!
     | IS_EQUAL
     | IS_LESS_OR_EQUAL
     | IS_MORE_OR_EQUAL
     | IS_NOT_EQUAL
     | IS_LESS
     | IS_MORE
     -- Reserved words
     | FOR
     | WHILE
     | IF
     | THEN
     | ELSE
     -- ID for functions
     | ID String
     deriving (Eq, Show)

hexToInt :: String -> Int
hexToInt (_:_:hex) = sum [ y * 16^x  | (x, y) <- zip [0..] $ map digitToInt $ reverse hex ]

getTokens :: String -> [Token]
getTokens str = alexScanTokens str

}
