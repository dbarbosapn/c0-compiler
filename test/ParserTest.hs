{-# LANGUAGE TemplateHaskell #-}
module ParserTest where
import Lexer
import Parser
import AST
import Test.QuickCheck


prop_simple_main_test = parse (getTokens "\nint main(int argc, char argv)\n{\n  return 0;\n}\n\n") ===  Program [FuncDef TypeInt "main" [DefParam TypeInt "argc", DefParam TypeChar "argv"] [ReturnStatement (Just (IntValue 0))]]  


return []
runParserTests = $(verboseCheckAll)
