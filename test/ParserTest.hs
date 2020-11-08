{-# LANGUAGE TemplateHaskell #-}
module ParserTest where
import Lexer
import Parser
import AST
import Test.QuickCheck


prop_simple_main_test = parse (getTokens "\nint main()\n{\n  return 0;\n}\n\n") ===  Program []  


prop_calc_sqrt_sum = parse (getTokens "\nint main()\n{\n  int s, n;\n\n  s = 0;\n  n = 1;\n\n  while( n <= 10 )\n    {\n      s = s + n*n;\n      n = n + 1;\n    }\n\n  print_int(n);\n\n  return 0;\n}\n\n") ===  Program []  


prop_test_if = parse (getTokens "\nint main()\n{\n  if ( n <= 3*2 )\n    n = 2;\n\n  if ( 1 )\n    n = 3;\n\n  else\n    n=4;\n}\n\n") ===  Program []  


prop_test_prime = parse (getTokens "\nbool is_prime(int n) {\n  int d;\n  d = 2;\n  if (n == 1) // 1 não é primo\n    return false;\n  while (d <= n) {\n    if (n%d == 0)\n      return false;\n    else\n      d = d+1;\n  }\n  return true;\n}\n\nint main() {\n  int n;\n  n = scan_int();\n  if (is_prime(n))\n    print_str(prime);\n  else\n    print_str(not_prime);\n}\n\n") ===  Program []  


prop_fac_calc = parse (getTokens "\nint factorial(int n) {\n  if (n == 0)\n    return 1;\n  return n * factorial(n-1);\n}\n\nint main() {\n  print_int(factorial(read_int()));\n}\n\n") ===  Program []  


return []
runParserTests = $(verboseCheckAll)
