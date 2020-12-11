{-# LANGUAGE TemplateHaskell #-}
module MiddleCodeTest where
import Lexer
import Parser
import MiddleCode
import Test.QuickCheck
import TestPrograms

import           Data.Map(Map)
import qualified Data.Map as Map


prop_arithmetic = transProgram (parse (getTokens "int main(int x, int y) { return x+1; }")) === [Def "main" ["t0","t1"] [MOVE "t3" "t0",MOVEI "t4" 1,OP A_ADD "t2" "t3" "t4",RETVAL "t2"] ]

prop_for = transProgram (parse (getTokens "int main(int x, int y) { for( int i=0; i<x; i = i+1 ) { y = y + 1;}}")) === [Def "main" ["t0","t1"] [MOVEI "t2" 0,LABEL "L0",MOVE "t3" "t2",MOVE "t4" "t0",COND "t3" R_LESS "t4" "L1" "L2",LABEL "L1",MOVE "t5" "t1",MOVEI "t6" 1,OP A_ADD "t1" "t5" "t6",MOVE "t7" "t2",MOVEI "t8" 1,OP A_ADD "t2" "t7" "t8",JUMP "L0",LABEL "L2"]]

prop_while = transProgram (parse (getTokens "int main(int x, int y) { while(x < y) { x = x + 1; } return 0; }")) === [Def "main" ["t0","t1"] [LABEL "L0",MOVE "t2" "t0",MOVE "t3" "t1",COND "t2" R_LESS "t3" "L1" "L2",LABEL "L1",MOVE "t4" "t0",MOVEI "t5" 1,OP A_ADD "t0" "t4" "t5",JUMP "L0",LABEL "L2",MOVEI "t6" 0,RETVAL "t6"]]

prop_if = transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;}}")) === [Def "main" ["t0","t1"] [MOVE "t2" "t0",MOVE "t3" "t1",COND "t2" R_LESS "t3" "L0" "L1",LABEL "L0",MOVE "t4" "t1",MOVEI "t5" 1,OP A_ADD "t1" "t4" "t5",LABEL "L1"]]

prop_if_else = transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;} else { x = x+1; }}")) === [Def "main" ["t0","t1"] [MOVE "t2" "t0",MOVE "t3" "t1",COND "t2" R_LESS "t3" "L0" "L1",LABEL "L0",MOVE "t4" "t1",MOVEI "t5" 1,OP A_ADD "t1" "t4" "t5",JUMP "L2",LABEL "L1",MOVE "t6" "t0",MOVEI "t7" 1,OP A_ADD "t0" "t6" "t7",LABEL "L2"]]

prop_if_elseif = transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;} else if(x>y) { x = x+1; } }")) === [Def "main" ["t0","t1"] [MOVE "t2" "t0",MOVE "t3" "t1",COND "t2" R_LESS "t3" "L0" "L1",LABEL "L0",MOVE "t4" "t1",MOVEI "t5" 1,OP A_ADD "t1" "t4" "t5",JUMP "L2",LABEL "L1",MOVE "t6" "t0",MOVE "t7" "t1",COND "t6" R_GREATER "t7" "L3" "L4",LABEL "L3",MOVE "t8" "t0",MOVEI "t9" 1,OP A_ADD "t0" "t8" "t9",LABEL "L4",LABEL "L2"]]

prop_relational_expr = transProgram (parse (getTokens "int main(int x, int y) { bool b = x < y; if(b) {return 0;} return 1; }")) === [Def "main" ["t0","t1"] [MOVE "t3" "t0",MOVE "t4" "t1",COND "t3" R_LESS "t4" "L0" "L1",LABEL "L0",MOVEI "t2" 1,LABEL "L1",MOVEI "t2" 0,MOVE "t5" "t2",COND_ZERO "t5" "L2" "L3",LABEL "L2",MOVEI "t6" 0,RETVAL "t6",LABEL "L3",MOVEI "t7" 1,RETVAL "t7"]]

prop_square_sum = transProgram (parse (getTokens test_program_square_sum)) === [Def "main" [] [MOVEI "t0" 0,MOVEI "t1" 1,LABEL "L0",MOVE "t2" "t1",MOVEI "t3" 10,COND "t2" R_LESSEQ "t3" "L1" "L2",LABEL "L1",MOVE "t4" "t0",MOVE "t6" "t1",MOVE "t7" "t1",OP A_MULT "t5" "t6" "t7",OP A_ADD "t0" "t4" "t5",MOVE "t8" "t1",MOVEI "t9" 1,OP A_ADD "t1" "t8" "t9",JUMP "L0",LABEL "L2",MOVE "t11" "t1",CALL "print_int" ["t11"]]]

prop_is_prime = transProgram (parse (getTokens test_program_integer_is_prime)) === [Def "is_prime" ["t0"] [MOVEI "t1" 2,MOVE "t2" "t0",MOVEI "t3" 1,COND "t2" R_EQ "t3" "L0" "L1",LABEL "L0",MOVEI "t4" 0,RETVAL "t4",LABEL "L1",LABEL "L2",MOVE "t5" "t1",MOVE "t6" "t0",COND "t5" R_LESSEQ "t6" "L3" "L4",LABEL "L3",MOVE "t9" "t0",MOVE "t10" "t1",OP A_MOD "t7" "t9" "t10",MOVEI "t8" 0,COND "t7" R_EQ "t8" "L5" "L6",LABEL "L5",MOVEI "t11" 0,RETVAL "t11",JUMP "L7",LABEL "L6",MOVE "t12" "t1",MOVEI "t13" 1,OP A_ADD "t1" "t12" "t13",LABEL "L7",JUMP "L2",LABEL "L4",MOVEI "t14" 1,RETVAL "t14"],Def "main" [] [CALL "scan_int" [],MOVE "t2" "t0",CALL "is_prime" ["t2"],COND_ZERO "t1" "L8" "L9",LABEL "L8",MOVES "t4" "prime",CALL "print_str" ["t4"],JUMP "L10",LABEL "L9",MOVES "t6" "not prime",CALL "print_str" ["t6"],LABEL "L10"]]

prop_factorial = transProgram (parse (getTokens test_program_factorial)) === [Def "factorial" ["t0"] [MOVE "t1" "t0",MOVEI "t2" 0,COND "t1" R_EQ "t2" "L0" "L1",LABEL "L0",MOVEI "t3" 1,RETVAL "t3",LABEL "L1",MOVE "t5" "t0",MOVE "t8" "t0",MOVEI "t9" 1,OP A_SUB "t7" "t8" "t9",CALL "factorial" ["t7"],OP A_MULT "t4" "t5" "t6",RETVAL "t4"],Def "main" [] [CALL "read_int" [],CALL "factorial" ["t2"],CALL "print_int" ["t1"]]]

return []
runMiddleCodeTest = $(verboseCheckAll)
