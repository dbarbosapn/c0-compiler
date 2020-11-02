-- Test all properties in the current module, using Template Haskell.
-- You need to have a {-# LANGUAGE TemplateHaskell #-} pragma in your
-- module for any of these to work.
{-# LANGUAGE TemplateHaskell #-}

module LexerTest where

import Lexer
import Test.QuickCheck

-- White space teste
prop_whiteSpace = getTokens " \n\r\t" === []

-- Separators test
prop_specialChars = getTokens "(){};[]," === [LPAREN, RPAREN, LBRACE, RBRACE, SEMICOLON, LBRACKET, RBRACKET, COMMA]

-- Comments
prop_lineComment = getTokens "//comments\n123" === [INT 123]
prop_multiLineComment = getTokens "/*Comments\nmoreComments*/123" === [INT 123]

-- Int test
prop_int = getTokens "123 123" === [INT 123, INT 123]

-- Hex Number test
prop_hex = getTokens "0xAbCdE123" === [INT 2882396451]

-- Bool Test
prop_bool = getTokens "true false" === [BOOL True, BOOL False]

-- Operators test
prop_ope = getTokens "+-/*%" === [A_OP ADD, A_OP SUB, A_OP DIV, A_OP MULT, A_OP MOD]
prop_ope1 = getTokens "= == <= >= != < >" === [ASSIGN, R_OP EQUAL, R_OP LESS_OR_EQUAL, R_OP MORE_OR_EQUAL, R_OP NOT_EQUAL, R_OP LESS, R_OP MORE]

-- Char test
prop_char = getTokens "\'a\'" === [CHAR 'a']

-- String test
prop_string = getTokens "\"string\"" === [STRING "string"]

-- Reserved Words
prop_reservedWords = getTokens "for while if then else return" === [FOR, WHILE, IF, THEN, ELSE, RETURN]

-- Id test
prop_id = getTokens "random while random whilerandom" === [ID "random", WHILE, ID "random", ID "whilerandom"]

-- Function Test
prop_function = 
    getTokens "int main(int argc, char argv){ return 0; }" === [T_INT, ID "main", LPAREN, T_INT, ID "argc", COMMA, T_CHAR, ID "argv", RPAREN, LBRACE, RETURN, INT 0, SEMICOLON, RBRACE]

return []
runLexerTests = $(verboseCheckAll)
