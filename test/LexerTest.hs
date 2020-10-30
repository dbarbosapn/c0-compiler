-- Test all properties in the current module, using Template Haskell.
-- You need to have a {-# LANGUAGE TemplateHaskell #-} pragma in your
-- module for any of these to work.
{-# LANGUAGE TemplateHaskell #-}

module LexerTest where

import Lexer
import Test.QuickCheck

prop_whiteSpace = getTokens " \n\r\t" === []

prop_specialChars = getTokens "(){}" === [LPAREN, RPAREN, LBRACE, RBRACE]


-- Comments
prop_lineComment = getTokens "//comments\n123" === [INT 123]
prop_multiLineComment = getTokens "/*Comments\nmoreComments*/123" === [INT 123]

prop_int = getTokens "123 123" === [INT 123, INT 123]

prop_bool = getTokens "true false" === [BOOL True, BOOL False]

prop_ope = getTokens "+-/*" === [ADD, SUB, DIV, MULT]

-- the bizarre return [] is needed on GHC 7.8
-- and later; without it, quickCheckAll will not be able to find
-- any of the properties. Weird stuff
return []
runLexerTests = $(quickCheckAll)
