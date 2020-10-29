{ module Lexic.Lexer where } -- What is between { } is actual haskell code

%wrapper "basic"

$digit = [0-9] -- digits
$alpha = [_a-zA-Z] -- alphabetc characters

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

{
data Token = LPAREN
     | RPAREN
     | COMMA
     | LBRACE
     | RBRACE
     | POINT
     deriving (Eq, Show)

getTokens :: String -> [Token]
getTokens str = alexScanTokens str

}
