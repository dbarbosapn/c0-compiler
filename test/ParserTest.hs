{-# LANGUAGE TemplateHaskell #-}
module ParserTest where
import Lexer
import Parser
import AST
import Test.QuickCheck


prop_simple_main_test = parse (getTokens "\nint main()\n{\n  return 0;\n}\n\n") ===  Program []  


return []
runParserTests = $(verboseCheckAll)
