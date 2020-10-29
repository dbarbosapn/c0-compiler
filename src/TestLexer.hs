{-# LANGUAGE TemplateHaskell #-}
module Lexic.TestLexer where

import Lexer
import Test.QuickCheck

-- White Space tester
whiteSpace = getTokens " \n \t " === []

-- Special chars tester
specialChars = getTokens "(){},." === [LPAREN, RPAREN, LBRACE, RBRACE, COMMA, POINT]


-- the bizarre return [] is needed on GHC 7.8
-- and later; without it, quickCheckAll will not be able to find
-- any of the properties. Weird stuff
return []
runTests = $quickCheckAll
