-- Test all properties in the current module, using Template Haskell.
-- You need to have a {-# LANGUAGE TemplateHaskell #-} pragma in your
-- module for any of these to work.
{-# LANGUAGE TemplateHaskell #-}

module TestLexer where

import Lexer
import Test.QuickCheck

prop_whiteSpace = getTokens " \n\r\t" == []

prop_specialChars = getTokens "(){},." == [LPAREN,RPAREN,LBRACE,RBRACE,COMMA,POINT]

prop_lineComment = getTokens "//comments\n." == [POINT]

prop_multLineComment = getTokens "/*Comments\nmoreComments*/." == [POINT]

prop_int = getTokens ",123,123" == [COMMA, NUM 123, COMMA, NUM 123]

prop_real = getTokens "1.23.123" == [REAL 1.23,POINT, NUM 123]
prop_real1 = getTokens "1.123e12 123e-12" == [REAL 1.123e12, REAL 1.23e-10]

prop_bool = getTokens "true FalsE False True" == [BOOL "true", BOOL "false", BOOL "false", BOOL "true"]

prop_ope = getTokens "+-/*" == [ADD, SUB, DIV, MULT]

-- the bizarre return [] is needed on GHC 7.8
-- and later; without it, quickCheckAll will not be able to find
-- any of the properties. Weird stuff
return []
runTests = $quickCheckAll
