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
"," { \_ -> COMMA }
"." { \_ -> POINT }
")" { \_ -> RPAREN }
"(" { \_ -> LPAREN }

-- Float and int numbers
$digit+ { \s -> NUM (read s :: Int) }
$digit+("."$digit+)?([eE][\-\+]?$digit+)? { \s -> REAL (read s :: Float) }

-- Bool type
[Tt][rR][uU][eE] { \s -> BOOL $ map toLower s }
[Ff][Aa][Ll][Ss][Ee] { \s -> BOOL $ map toLower s }

-- Operators
"+" { \_ -> ADD }
"-" { \_ -> SUB }
"/" { \_ -> DIV }
"*" { \_ -> MULT }

{
data Token = LPAREN
     | RPAREN
     | COMMA
     | LBRACE
     | RBRACE
     | POINT
     | NUM Int
     | REAL Float
     | BOOL String
     | ADD
     | SUB
     | DIV
     | MULT
     deriving (Eq, Show)

getTokens :: String -> [Token]
getTokens str = alexScanTokens str

}
