{-# LANGUAGE TemplateHaskell #-}
module MiddleCodeTest where
import Lexer
import Parser
import MiddleCode
import Test.QuickCheck
import TestPrograms

import           Data.Map(Map)
import qualified Data.Map as Map


prop_empty_main = transProgram (parse (getTokens "int main(int x, int y) { return x+1; }")) === []

prop_for = transProgram (parse (getTokens "int main(int x, int y) { for( int i=0; i<x; i = i+1 ) { y = y + 1;}}")) === []

return []
runMiddleCodeTest = $(verboseCheckAll)
