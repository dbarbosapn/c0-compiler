-- Test all properties in the current module, using Template Haskell.
-- You need to have a {-# LANGUAGE TemplateHaskell #-} pragma in your
-- module for any of these to work.
{-# LANGUAGE TemplateHaskell #-}

module ParserTest where

import Lexer
import Parser
import Test.QuickCheck

return []
runParserTests = $(verboseCheckAll)