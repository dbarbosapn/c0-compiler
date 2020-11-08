{-# LANGUAGE TemplateHaskell #-}
module ParserTest where
import Lexer
import Parser
import AST
import Test.QuickCheck


prop_simple_main_test = parse (getTokens "\nint main()\n{\n  return 0;\n}\n\n") ===  Program []  


prop_simple_var_definition = parse (getTokens "\nint main()\n{\n  int s, n;\n\n  s = 0;\n  n = 1;\n\n  while( n <= 10 )\n    {\n      s = s + n*n;\n      n = n + 1;\n    }\n\n  print_int(n);\n\n  return 0;\n}\n\n") ===  Program []  


return []
runParserTests = $(verboseCheckAll)
