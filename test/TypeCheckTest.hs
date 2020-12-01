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

return []
runTypeCheckTest = $(verboseCheckAll)
