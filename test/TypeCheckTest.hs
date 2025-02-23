{-# LANGUAGE TemplateHaskell #-}
module TypeCheckTest where
import Lexer
import Parser
import TypeCheck
import Test.QuickCheck
import TestPrograms

import           Data.Map(Map)
import qualified Data.Map as Map


prop_program_TypeCheckFail = checkProgram (parse (getTokens test_program_typeCheckFail)) === False

prop_program_square_sum = checkProgram (parse (getTokens test_program_square_sum))

prop_program_integer_is_prime = checkProgram (parse (getTokens test_program_integer_is_prime))

prop_program_factorial = checkProgram (parse (getTokens test_program_factorial))

return []
runTypeCheckTest = $(verboseCheckAll)
