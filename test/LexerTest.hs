-- Test all properties in the current module, using Template Haskell.
-- You need to have a {-# LANGUAGE TemplateHaskell #-} pragma in your
-- module for any of these to work.
{-# LANGUAGE TemplateHaskell #-}

module LexerTest where

import Lexer
import Test.QuickCheck

-- White space teste
prop_whiteSpace = getTokens " \n\r\t" === []

-- Special characters test
prop_specialChars = getTokens "(){};[]" === [LPAREN, RPAREN, LBRACE, RBRACE, SEMICOLON, LBRACKET, RBRACKET]

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
prop_ope = getTokens "+-/*%" === [ADD, SUB, DIV, MULT, MOD]
prop_ope1 = getTokens "= == <= >= /= < > " === [EQUAL, IS_EQUAL, IS_LESS_OR_EQUAL, IS_MORE_OR_EQUAL, IS_NOT_EQUAL, IS_LESS, IS_MORE]

-- Char test
prop_char = getTokens "\'a\'" === [CHAR 'a']

-- String test
prop_string = getTokens "\"string\"" === [STRING "string"]

-- Special Words
prop_reservedWords = getTokens "for while if then else" === [FOR, WHILE, IF, THEN, ELSE]

-- Id test
prop_id = getTokens "random while random whilerandom" === [ID "random",WHILE,ID "random",ID "whilerandom"]


-- the bizarre return [] is needed on GHC 7.8
-- and later; without it, quickCheckAll will not be able to find
-- any of the properties. Weird stuff
return []
runLexerTests = $(verboseCheckAll)
