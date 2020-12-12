{-# LANGUAGE TemplateHaskell #-}
module MachineSymTest where
import Lexer
import Parser
import MiddleCode
import MachineSym
import Test.QuickCheck
import TestPrograms

prop_arithmetic = generateMips (transProgram (parse (getTokens "int main(int x, int y) { return x+1; }"))) === "_main_:\n\tmove $t1, $a0\n\tli $t2, 1\n\tadd $t0, $t1, $t2\n\tmove $v0, $t0\n\tjr $ra\tjr $ra\n"

prop_for = generateMips (transProgram (parse (getTokens "int main(int x, int y) { for( int i=0; i<x; i = i+1 ) { y = y + 1;}}"))) === "_main_:\n\tli $s0, 0\nL0:\n\tmove $t0, $s0\n\tmove $t1, $a0\n\tbge $t0, $t1, L2\nL1:\n\tmove $t0, $a1\n\tli $t1, 1\n\tadd $a1, $t0, $t1\n\tmove $t0, $s0\n\tli $t1, 1\n\tadd $s0, $t0, $t1\n\tjump L0\nL2:\n\tjr $ra\n"

prop_while = generateMips (transProgram (parse (getTokens "int main(int x, int y) { while(x < y) { x = x + 1; } return 0; }"))) === "_main_:\nL0:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tbge $t0, $t1, L2\nL1:\n\tmove $t0, $a0\n\tli $t1, 1\n\tadd $a0, $t0, $t1\n\tjump L0\nL2:\n\tli $t0, 0\n\tmove $v0, $t0\n\tjr $ra\tjr $ra\n"

prop_if = generateMips (transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;}}"))) === "_main_:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tbge $t0, $t1, L1\nL0:\n\tmove $t0, $a1\n\tli $t1, 1\n\tadd $a1, $t0, $t1\nL1:\n\tjr $ra\n"

prop_if_else = generateMips (transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;} else { x = x+1; }}"))) === "_main_:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tbge $t0, $t1, L1\nL0:\n\tmove $t0, $a1\n\tli $t1, 1\n\tadd $a1, $t0, $t1\n\tjump L2\nL1:\n\tmove $t0, $a0\n\tli $t1, 1\n\tadd $a0, $t0, $t1\nL2:\n\tjr $ra\n"

prop_if_elseif = generateMips (transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;} else if(x>y) { x = x+1; } }"))) === "_main_:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tbge $t0, $t1, L1\nL0:\n\tmove $t0, $a1\n\tli $t1, 1\n\tadd $a1, $t0, $t1\n\tjump L2\nL1:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tble $t0, $t1, L4\nL3:\n\tmove $t0, $a0\n\tli $t1, 1\n\tadd $a0, $t0, $t1\nL4:\nL2:\n\tjr $ra\n"

prop_relational_expr = generateMips (transProgram (parse (getTokens "int main(int x, int y) { bool b = x < y; if(b) {return 0;} return 1; }"))) === "_main_:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tbge $t0, $t1, L1\nL0:\n\tli $s0, 1\n\tjump L2\nL1:\n\tli $s0, 0\nL2:\n\tmove $t0, $s0\n\tbeq t0, $zero, L4\nL3:\n\tli $t0, 0\n\tmove $v0, $t0\n\tjr $ra\nL4:\n\tli $t1, 1\n\tmove $v0, $t1\n\tjr $ra\n\tjr $ra\n"

prop_square_sum = generateMips (transProgram (parse (getTokens test_program_square_sum))) === ""

prop_is_prime = generateMips (transProgram (parse (getTokens test_program_integer_is_prime))) === ""

prop_factorial = generateMips (transProgram (parse (getTokens test_program_factorial))) === ""

return []
runMachineSymTest = $(verboseCheckAll)