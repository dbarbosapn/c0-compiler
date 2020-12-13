{-# LANGUAGE TemplateHaskell #-}
module MachineSymTest where
import Lexer
import Parser
import MiddleCode
import MachineSym
import Test.QuickCheck
import TestPrograms

prop_arithmetic = generateMips (transProgram (parse (getTokens "int main(int x, int y) { return x+1; }"))) === "\tjal _main_\n\tli $v0, 10\n\tsyscall\n_main_:\n\tmove $t1, $a0\n\tli $t2, 1\n\tadd $t0, $t1, $t2\n\tmove $v0, $t0\n\tjr $ra\n\tjr $ra\nprint_int:\n\tli $2, 1\n\tsyscall\n\tjr $ra\n"

prop_for = generateMips (transProgram (parse (getTokens "int main(int x, int y) { for( int i=0; i<x; i = i+1 ) { y = y + 1;}}"))) === "\tjal _main_\n\tli $v0, 10\n\tsyscall\n_main_:\n\tli $s0, 0\nL0:\n\tmove $t0, $s0\n\tmove $t1, $a0\n\tbge $t0, $t1, L2\nL1:\n\tmove $t0, $a1\n\tli $t1, 1\n\tadd $a1, $t0, $t1\n\tmove $t0, $s0\n\tli $t1, 1\n\tadd $s0, $t0, $t1\n\tj L0\nL2:\n\tjr $ra\nprint_int:\n\tli $2, 1\n\tsyscall\n\tjr $ra\n"

prop_while = generateMips (transProgram (parse (getTokens "int main(int x, int y) { while(x < y) { x = x + 1; } return 0; }"))) === "\tjal _main_\n\tli $v0, 10\n\tsyscall\n_main_:\nL0:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tbge $t0, $t1, L2\nL1:\n\tmove $t0, $a0\n\tli $t1, 1\n\tadd $a0, $t0, $t1\n\tj L0\nL2:\n\tli $t0, 0\n\tmove $v0, $t0\n\tjr $ra\n\tjr $ra\nprint_int:\n\tli $2, 1\n\tsyscall\n\tjr $ra\n"

prop_if = generateMips (transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;}}"))) === "\tjal _main_\n\tli $v0, 10\n\tsyscall\n_main_:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tbge $t0, $t1, L1\nL0:\n\tmove $t0, $a1\n\tli $t1, 1\n\tadd $a1, $t0, $t1\nL1:\n\tjr $ra\nprint_int:\n\tli $2, 1\n\tsyscall\n\tjr $ra\n"

prop_if_else = generateMips (transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;} else { x = x+1; }}"))) === "\tjal _main_\n\tli $v0, 10\n\tsyscall\n_main_:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tbge $t0, $t1, L1\nL0:\n\tmove $t0, $a1\n\tli $t1, 1\n\tadd $a1, $t0, $t1\n\tj L2\nL1:\n\tmove $t0, $a0\n\tli $t1, 1\n\tadd $a0, $t0, $t1\nL2:\n\tjr $ra\nprint_int:\n\tli $2, 1\n\tsyscall\n\tjr $ra\n"

prop_if_elseif = generateMips (transProgram (parse (getTokens "int main(int x, int y) { if(x<y) { y = y + 1;} else if(x>y) { x = x+1; } }"))) === "\tjal _main_\n\tli $v0, 10\n\tsyscall\n_main_:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tbge $t0, $t1, L1\nL0:\n\tmove $t0, $a1\n\tli $t1, 1\n\tadd $a1, $t0, $t1\n\tj L2\nL1:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tble $t0, $t1, L4\nL3:\n\tmove $t0, $a0\n\tli $t1, 1\n\tadd $a0, $t0, $t1\nL4:\nL2:\n\tjr $ra\nprint_int:\n\tli $2, 1\n\tsyscall\n\tjr $ra\n"

prop_relational_expr = generateMips (transProgram (parse (getTokens "int main(int x, int y) { bool b = x < y; if(b) {return 0;} return 1; }"))) === "\tjal _main_\n\tli $v0, 10\n\tsyscall\n_main_:\n\tmove $t0, $a0\n\tmove $t1, $a1\n\tbge $t0, $t1, L1\nL0:\n\tli $s0, 1\n\tj L2\nL1:\n\tli $s0, 0\nL2:\n\tmove $t0, $s0\n\tbeq t0, $zero, L4\nL3:\n\tli $t0, 0\n\tmove $v0, $t0\n\tjr $ra\nL4:\n\tli $t1, 1\n\tmove $v0, $t1\n\tjr $ra\n\tjr $ra\nprint_int:\n\tli $2, 1\n\tsyscall\n\tjr $ra\n"

prop_square_sum = generateMips (transProgram (parse (getTokens test_program_square_sum))) === "\tjal _main_\n\tli $v0, 10\n\tsyscall\n_main_:\n\tli $s0, 0\n\tli $s1, 1\nL0:\n\tmove $t0, $s1\n\tli $t1, 10\n\tbgt $t0, $t1, L2\nL1:\n\tmove $t0, $s0\n\tmove $t2, $s1\n\tmove $t3, $s1\n\tmul $t1, $t2, $t3\n\tadd $s0, $t0, $t1\n\tmove $t0, $s1\n\tli $t1, 1\n\tadd $s1, $t0, $t1\n\tj L0\nL2:\n\tmove $t1, $s0\n\taddiu $sp, $sp, -8\n\tsw $ra, 4($sp)\n\tsw $t0, 0($sp)\n\tmove $a0, $t1\n\tjal print_int\n\tlw $ra, 4($sp)\n\tlw $t0, 0($sp)\n\taddiu $sp, $sp, 8\n\tmove $t0, $v0\n\tjr $ra\nprint_int:\n\tli $2, 1\n\tsyscall\n\tjr $ra\n"

prop_is_prime = generateMips (transProgram (parse (getTokens test_program_integer_is_prime))) === ""

prop_factorial = generateMips (transProgram (parse (getTokens test_program_factorial))) === ""

return []
runMachineSymTest = $(verboseCheckAll)