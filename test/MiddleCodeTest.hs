{-# LANGUAGE TemplateHaskell #-}
module MiddleCodeTest where
import Lexer
import Parser
import MiddleCode
import Test.QuickCheck
import TestPrograms

import           Data.Map(Map)
import qualified Data.Map as Map


prop_arithmetic = transProgram (parse (getTokens "int main(int x, int y) { return x+1; }")) === [Def "main" ["t0","t1"] [MOVE "t3" "t0",MOVEI "t4" 1,OP A_ADD "t2" "t3" "t4",RETVAL "t2"]]

prop_for = transProgram (parse (getTokens "int main(int x, int y) { for( int i=0; i<x; i = i+1 ) { y = y + 1;}}")) === [Def "main" ["t0","t1"] [MOVEI "t2" 0,LABEL "L0",MOVE "t4" "t2",MOVE "t5" "t0",OP R_LESS "t3" "t4" "t5",COND "t3" "L1" "L2",LABEL "L1",MOVE "t6" "t1",MOVEI "t7" 1,OP A_ADD "t1" "t6" "t7",MOVE "t8" "t2",MOVEI "t9" 1,OP A_ADD "t2" "t8" "t9",JUMP "L0",LABEL "L2"]]

prop_while = transProgram (parse (getTokens "int main(int x, int y) { while(x < y) { x = x + 1; } return 0; }")) === [Def "main" ["t0","t1"] [LABEL "L0",MOVE "t3" "t0",MOVE "t4" "t1",OP R_LESS "t2" "t3" "t4",COND "t2" "L1" "L2",LABEL "L1",MOVE "t5" "t0",MOVEI "t6" 1,OP A_ADD "t0" "t5" "t6",JUMP "L0",LABEL "L2",MOVEI "t7" 0,RETVAL "t7"]]

prop_if = transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;}}")) === [Def "main" ["t0","t1"] [MOVE "t3" "t0",MOVE "t4" "t1",OP R_LESS "t2" "t3" "t4",COND "t2" "L0" "L1",LABEL "L0",MOVE "t5" "t1",MOVEI "t6" 1,OP A_ADD "t1" "t5" "t6",LABEL "L1"]]

prop_if_else = transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;} else { x = x+1; }}")) === [Def "main" ["t0","t1"] [MOVE "t3" "t0",MOVE "t4" "t1",OP R_LESS "t2" "t3" "t4",COND "t2" "L0" "L1",LABEL "L0",MOVE "t5" "t1",MOVEI "t6" 1,OP A_ADD "t1" "t5" "t6",JUMP "L2",LABEL "L1",MOVE "t7" "t0",MOVEI "t8" 1,OP A_ADD "t0" "t7" "t8",LABEL "L2"]]

prop_relational_expr = transProgram (parse (getTokens "int main(int x, int y) { bool b = x < y; if(b) {return 0;} return 1; }")) === [Def "main" ["t0","t1"] [MOVE "t3" "t0",MOVE "t4" "t1",OP R_LESS "t2" "t3" "t4",MOVE "t5" "t2",COND "t5" "L0" "L1",LABEL "L0",MOVEI "t6" 0,RETVAL "t6",LABEL "L1",MOVEI "t7" 1,RETVAL "t7"]]

prop_square_sum = transProgram (parse (getTokens test_program_square_sum)) === [Def "main" [] [MOVEI "t0" 0,MOVEI "t1" 1,LABEL "L0",MOVE "t3" "t1",MOVEI "t4" 10,OP R_LESSEQ "t2" "t3" "t4",COND "t2" "L1" "L2",LABEL "L1",MOVE "t5" "t0",MOVE "t7" "t1",MOVE "t8" "t1",OP A_MULT "t6" "t7" "t8",OP A_ADD "t0" "t5" "t6",MOVE "t9" "t1",MOVEI "t10" 1,OP A_ADD "t1" "t9" "t10",JUMP "L0",LABEL "L2",MOVE "t12" "t1",CALL "print_int" ["t12"]]]

prop_is_prime = transProgram (parse (getTokens test_program_integer_is_prime)) === [Def "is_prime" ["t0"] [MOVEI "t1" 2,MOVE "t3" "t0",MOVEI "t4" 1,OP R_EQ "t2" "t3" "t4",COND "t2" "L0" "L1",LABEL "L0",MOVEI "t5" 0,RETVAL "t5",LABEL "L1",LABEL "L2",MOVE "t7" "t1",MOVE "t8" "t0",OP R_LESSEQ "t6" "t7" "t8",COND "t6" "L3" "L4",LABEL "L3",MOVE "t12" "t0",MOVE "t13" "t1",OP A_MOD "t10" "t12" "t13",MOVEI "t11" 0,OP R_EQ "t9" "t10" "t11",COND "t9" "L5" "L6",LABEL "L5",MOVEI "t14" 0,RETVAL "t14",JUMP "L7",LABEL "L6",MOVE "t15" "t1",MOVEI "t16" 1,OP A_ADD "t1" "t15" "t16",LABEL "L7",JUMP "L2",LABEL "L4",MOVEI "t17" 1,RETVAL "t17"],Def "main" [] [CALL "scan_int" [],MOVE "t2" "t0",CALL "is_prime" ["t2"],COND "t1" "L8" "L9",LABEL "L8",MOVES "t4" "prime",CALL "print_str" ["t4"],JUMP "L10",LABEL "L9",MOVES "t6" "not prime",CALL "print_str" ["t6"],LABEL "L10"]]

prop_factorial = transProgram (parse (getTokens test_program_factorial)) === [Def "factorial" ["t0"] [MOVE "t2" "t0",MOVEI "t3" 0,OP R_EQ "t1" "t2" "t3",COND "t1" "L0" "L1",LABEL "L0",MOVEI "t4" 1,RETVAL "t4",LABEL "L1",MOVE "t6" "t0",MOVE "t9" "t0",MOVEI "t10" 1,OP A_SUB "t8" "t9" "t10",CALL "factorial" ["t8"],OP A_MULT "t5" "t6" "t7",RETVAL "t5"],Def "main" [] [CALL "read_int" [],CALL "factorial" ["t2"],CALL "print_int" ["t1"]]]

return []
runMiddleCodeTest = $(verboseCheckAll)
