{-# LANGUAGE TemplateHaskell #-}
module ParserTest where
import Lexer
import Parser
import AST
import Test.QuickCheck
import TestPrograms

prop_for = parse (getTokens "int main(int x, int y) { for( int i=0; i<x; i = i+1 ) { y = y + 1;}}") === Program [FuncDef TypeInt "main" [DefParam TypeInt "x",DefParam TypeInt "y"] (MultipleStatements [ForStatement (Just (VariableDeclaration TypeInt "i" (Just (IntValue 0))),BinaryOperation (RelationalOperation (IsLess (Id "i") (Id "x"))),Just (AssignOperation (Assign "i" (BinaryOperation (ArithmeticOperation (Add (Id "i") (IntValue 1))))))) (MultipleStatements [Simple (AssignOperation (Assign "y" (BinaryOperation (ArithmeticOperation (Add (Id "y") (IntValue 1))))))])])]

prop_program_square_sum = parse (getTokens test_program_square_sum) === Program [FuncDef TypeInt "main" [] (MultipleStatements [Simple (VariableDeclaration TypeInt "s" Nothing),Simple (VariableDeclaration TypeInt "n" Nothing),Simple (AssignOperation (Assign "s" (IntValue 0))),Simple (AssignOperation (Assign "n" (IntValue 1))),WhileStatement (BinaryOperation (RelationalOperation (IsLessOrEqual (Id "n") (IntValue 10)))) (MultipleStatements [Simple (AssignOperation (Assign "s" (BinaryOperation (ArithmeticOperation (Add (Id "s") (BinaryOperation (ArithmeticOperation (Multiply (Id "n") (Id "n"))))))))),Simple (AssignOperation (Assign "n" (BinaryOperation (ArithmeticOperation (Add (Id "n") (IntValue 1))))))]),Simple (Expression (FunctionCall "print_int" [Id "s"]))])]

prop_program_integer_is_prime = parse (getTokens test_program_integer_is_prime) === Program [FuncDef TypeBool "is_prime" [DefParam TypeInt "n"] (MultipleStatements [Simple (VariableDeclaration TypeInt "d" Nothing),Simple (AssignOperation (Assign "d" (IntValue 2))),IfStatement (BinaryOperation (RelationalOperation (Equals (Id "n") (IntValue 1)))) (ReturnStatement (Just (BoolValue False))),WhileStatement (BinaryOperation (RelationalOperation (IsLessOrEqual (Id "d") (Id "n")))) (MultipleStatements [IfElseStatement (BinaryOperation (RelationalOperation (Equals (BinaryOperation (ArithmeticOperation (Modulo (Id "n") (Id "d")))) (IntValue 0)))) (ReturnStatement (Just (BoolValue False))) (Simple (AssignOperation (Assign "d" (BinaryOperation (ArithmeticOperation (Add (Id "d") (IntValue 1)))))))]),ReturnStatement (Just (BoolValue True))]),FuncDef TypeInt "main" [] (MultipleStatements [Simple (VariableDeclaration TypeInt "n" Nothing),Simple (AssignOperation (Assign "n" (FunctionCall "scan_int" []))),IfElseStatement (FunctionCall "is_prime" [Id "n"]) (Simple (Expression (FunctionCall "print_str" [StringValue "prime"]))) (Simple (Expression (FunctionCall "print_str" [StringValue "not prime"])))])]

prop_program_TypeCheckFail = parse (getTokens test_program_typeCheckFail) === Program [FuncDef TypeInt "main" [] (MultipleStatements [Simple (VariableDeclaration TypeInt "s" (Just (BoolValue True)))])]

prop_program_factorial = parse (getTokens test_program_factorial) === Program [FuncDef TypeInt "factorial" [DefParam TypeInt "n"] (MultipleStatements [IfStatement (BinaryOperation (RelationalOperation (Equals (Id "n") (IntValue 0)))) (ReturnStatement (Just (IntValue 1))),ReturnStatement (Just (BinaryOperation (ArithmeticOperation (Multiply (Id "n") (FunctionCall "factorial" [BinaryOperation (ArithmeticOperation (Subtract (Id "n") (IntValue 1)))])))))]),FuncDef TypeInt "main" [] (MultipleStatements [Simple (Expression (FunctionCall "print_int" [FunctionCall "factorial" [FunctionCall "read_int" []]]))])]

return []
runParserTests = $(verboseCheckAll)
