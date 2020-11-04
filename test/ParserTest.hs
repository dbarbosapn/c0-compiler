-- Test all properties in the current module, using Template Haskell.
-- You need to have a {-# LANGUAGE TemplateHaskell #-} pragma in your
-- module for any of these to work.
{-# LANGUAGE TemplateHaskell #-}

module ParserTest where

import Lexer
import Parser
import AST
import Test.QuickCheck

prop_simple_main = 
    parse (getTokens "int main(int argc, char argv){ return 0; }") === Program [FuncDef TypeInt "main" [DefParam TypeInt "argc", DefParam TypeChar "argv"] [ReturnStatement (Just (IntValue 0))]]

return []
--runParserTests = $(verboseCheckAll)
runParserTests = $(quickCheckAll)
