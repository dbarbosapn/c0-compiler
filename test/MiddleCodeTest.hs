{-# LANGUAGE TemplateHaskell #-}
module MiddleCodeTest where
import Lexer
import Parser
import MiddleCode
import Test.QuickCheck
import TestPrograms

prop_arithmetic = transProgram (parse (getTokens "int main(int x, int y) { return x+1; }")) === [Def "main" ["a0","a1"] [MOVE "t1" "a0",MOVEI "t2" 1,OP A_ADD "t0" "t1" "t2",RETVAL "t0"]]

prop_for = transProgram (parse (getTokens "int main(int x, int y) { for( int i=0; i<x; i = i+1 ) { y = y + 1;}}")) === [Def "main" ["a0","a1"] [MOVEI "s0" 0,LABEL "L0",MOVE "t0" "s0",MOVE "t1" "a0",COND "t0" R_LESS "t1" "L1" "L2",LABEL "L1",MOVE "t0" "a1",MOVEI "t1" 1,OP A_ADD "a1" "t0" "t1",MOVE "t0" "s0",MOVEI "t1" 1,OP A_ADD "s0" "t0" "t1",JUMP "L0",LABEL "L2"]]

prop_while = transProgram (parse (getTokens "int main(int x, int y) { while(x < y) { x = x + 1; } return 0; }")) === [Def "main" ["a0","a1"] [LABEL "L0",MOVE "t0" "a0",MOVE "t1" "a1",COND "t0" R_LESS "t1" "L1" "L2",LABEL "L1",MOVE "t0" "a0",MOVEI "t1" 1,OP A_ADD "a0" "t0" "t1",JUMP "L0",LABEL "L2",MOVEI "t0" 0,RETVAL "t0"]]

prop_if = transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;}}")) === [Def "main" ["a0","a1"] [MOVE "t0" "a0",MOVE "t1" "a1",COND "t0" R_LESS "t1" "L0" "L1",LABEL "L0",MOVE "t0" "a1",MOVEI "t1" 1,OP A_ADD "a1" "t0" "t1",LABEL "L1"]]

prop_if_else = transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;} else { x = x+1; }}")) === [Def "main" ["a0","a1"] [MOVE "t0" "a0",MOVE "t1" "a1",COND "t0" R_LESS "t1" "L0" "L1",LABEL "L0",MOVE "t0" "a1",MOVEI "t1" 1,OP A_ADD "a1" "t0" "t1",JUMP "L2",LABEL "L1",MOVE "t0" "a0",MOVEI "t1" 1,OP A_ADD "a0" "t0" "t1",LABEL "L2"]]

prop_if_elseif = transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;} else if(x>y) { x = x+1; } }")) === [Def "main" ["a0","a1"] [MOVE "t0" "a0",MOVE "t1" "a1",COND "t0" R_LESS "t1" "L0" "L1",LABEL "L0",MOVE "t0" "a1",MOVEI "t1" 1,OP A_ADD "a1" "t0" "t1",JUMP "L2",LABEL "L1",MOVE "t0" "a0",MOVE "t1" "a1",COND "t0" R_GREATER "t1" "L3" "L4",LABEL "L3",MOVE "t0" "a0",MOVEI "t1" 1,OP A_ADD "a0" "t0" "t1",LABEL "L4",LABEL "L2"]]

prop_relational_expr = transProgram (parse (getTokens "int main(int x, int y) { bool b = x < y; if(b) {return 0;} return 1; }")) === [Def "main" ["a0","a1"] [MOVE "t0" "a0",MOVE "t1" "a1",COND "t0" R_LESS "t1" "L0" "L1",LABEL "L0",MOVEI "s0" 1,JUMP "L2",LABEL "L1",MOVEI "s0" 0,LABEL "L2",MOVE "t0" "s0",COND_ZERO "t0" "L3" "L4",LABEL "L3",MOVEI "t0" 0,RETVAL "t0",LABEL "L4",MOVEI "t1" 1,RETVAL "t1"]]

prop_square_sum = transProgram (parse (getTokens test_program_square_sum)) === [Def "main" [] [MOVEI "s0" 0,MOVEI "s1" 1,LABEL "L0",MOVE "t0" "s1",MOVEI "t1" 10,COND "t0" R_LESSEQ "t1" "L1" "L2",LABEL "L1",MOVE "t0" "s0",MOVE "t2" "s1",MOVE "t3" "s1",OP A_MULT "t1" "t2" "t3",OP A_ADD "s0" "t0" "t1",MOVE "t0" "s1",MOVEI "t1" 1,OP A_ADD "s1" "t0" "t1",JUMP "L0",LABEL "L2",MOVE "t1" "s1",CALL "print_int" ["t1"],MOVE_RET "t0"]]

prop_is_prime = transProgram (parse (getTokens test_program_integer_is_prime)) === [Def "is_prime" ["a0"] [MOVEI "s0" 2,MOVE "t0" "a0",MOVEI "t1" 1,COND "t0" R_EQ "t1" "L0" "L1",LABEL "L0",MOVEI "t0" 0,RETVAL "t0",LABEL "L1",LABEL "L2",MOVE "t1" "s0",MOVE "t2" "a0",COND "t1" R_LESSEQ "t2" "L3" "L4",LABEL "L3",MOVE "t3" "a0",MOVE "t4" "s0",OP A_MOD "t1" "t3" "t4",MOVEI "t2" 0,COND "t1" R_EQ "t2" "L5" "L6",LABEL "L5",MOVEI "t1" 0,RETVAL "t1",JUMP "L7",LABEL "L6",MOVE "t2" "s0",MOVEI "t3" 1,OP A_ADD "s0" "t2" "t3",LABEL "L7",JUMP "L2",LABEL "L4",MOVEI "t2" 1,RETVAL "t2"],Def "main" [] [CALL "scan_int" [],MOVE_RET "s0",MOVE "t1" "s0",CALL "is_prime" ["t1"],MOVE_RET "t0",COND_ZERO "t0" "L8" "L9",LABEL "L8",MOVES "t2" "prime",CALL "print_str" ["t2"],MOVE_RET "t1",JUMP "L10",LABEL "L9",MOVES "t4" "not prime",CALL "print_str" ["t4"],MOVE_RET "t3",LABEL "L10"]]

prop_factorial = transProgram (parse (getTokens test_program_factorial)) === [Def "factorial" ["a0"] [MOVE "t0" "a0",MOVEI "t1" 0,COND "t0" R_EQ "t1" "L0" "L1",LABEL "L0",MOVEI "t0" 1,RETVAL "t0",LABEL "L1",MOVE "t2" "a0",MOVE "t5" "a0",MOVEI "t6" 1,OP A_SUB "t4" "t5" "t6",CALL "factorial" ["t4"],MOVE_RET "t3",OP A_MULT "t1" "t2" "t3",RETVAL "t1"],Def "main" [] [CALL "read_int" [],MOVE_RET "t2",CALL "factorial" ["t2"],MOVE_RET "t1",CALL "print_int" ["t1"],MOVE_RET "t0"]]

return []
runMiddleCodeTest = $(verboseCheckAll)