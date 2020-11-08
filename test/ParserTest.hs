{-# LANGUAGE TemplateHaskell #-}
module ParserTest where
import Lexer
import Parser
import AST
import Test.QuickCheck


prop_simple_main_test = "\nint main()\n{\n  return 0;\n}\n\n" ===  Program [FuncDef TypeInt "main" [DefParam TypeInt "argc", DefParam TypeChar "argv"] [ReturnStatement (Just (IntValue 0))]]  


prop_simple_sqrt_calc = "\nint main() {\n  int s, n;\n  s = 0;\n  n = 1;\n  while (n <= 10) {\n    s = s + n*n;\n    n = n + 1;\n  }\n  print_int(n);\n}\n\n" ===  []  


return []
runParserTests = $(verboseCheckAll
