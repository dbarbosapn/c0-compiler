module Typecheck where

import           AST

import           Data.Map(Map)
import qualified Data.Map as Map

-- Tabela de simbolos
type SymTable = Map String Type


-- Expressions

checkExpr :: SymTable -> Expression -> Type
-- Basic types
checkExpr table (IntValue _) = TypeInt
checkExpr table (BoolValue _) = TypeBool
checkExpr table (StringValue _) = TypeChar
checkExpr table (CharValue _) = TypeChar
-- Variable recognition

checkExpr table (Id something) =
  case Map.lookup something table of
    Nothing -> error "undeclared variable"
    Just t -> t

checkExpr table (BinaryOperation (ArithmeticOperation something)) =
  let
    (t1, t2) = case something of
                 (Add e1 e2) -> (e1, e2)
                 (Subtract e1 e2) -> (e1, e2)
                 (Divide e1 e2) -> (e1, e2)
                 (Multiply e1 e2) -> (e1, e2)
                 (Modulo e1 e2) -> (e1, e2)
    t1' = checkExpr table t1
    t2' = checkExpr table t2
  in
    if t1' == TypeInt && t2' == TypeInt
    then
      TypeInt
    else
       error "Type error in ArithmeticOperation. Invalid type"

-- Atenção eu estou a supor que so comparamos numeros e não strings
checkExpr table (BinaryOperation (RelationalOperation something)) =
  let
    (t1, t2) = case something of
                 (Equals e1 e2) -> (e1, e2)
                 (IsLessOrEqual e1 e2) -> (e1, e2)
                 (IsMoreOrEqual e1 e2) -> (e1, e2)
                 (IsNotEqual e1 e2) -> (e1, e2)
                 (IsLess e1 e2) -> (e1, e2)
                 (IsMore e1 e2) -> (e1, e2)
    t1' = checkExpr table t1
    t2' = checkExpr table t2
  in
    if t1' == TypeInt && t2' == TypeInt
    then
      TypeBool
    else
       error "Type error in RelationalOperation. Invalid type"



-- Statements
checkStm :: SymTable -> Statement -> Bool
checkStm table (Simple something) =
  True
  where
    table1 = checkSimpleStm table something

checkStm table (IfStatement e1 stm1) =
  let t1 = checkExpr table e1
  in
    if t1 == TypeBool
    then
      checkStm table stm1
    else
       error "type error: condition must be bool"

checkStm table (IfElseStatement e1 stm1 stm2) =
  let t1 = checkStm table (IfStatement e1 stm1)
      t2 = checkStm table stm2
  in
    if t1 && t2
    then
      True
    else
      error "Something wrong with the ifelsestatement"

checkStm table (WhileStatement e1 stm1) =
  let t1 = checkExpr table e1
  in
    if t1 == TypeBool
    then
      checkStm table stm1
    else
       error "type error: condition must be bool"

checkStm table (ForStatement (something1, e1, something2) stm) =
  let t1 = checkExpr table e1
      table' = case something1 of
                 Nothing -> table
                 Just some1 -> checkSimpleStm table some1
      table'' = case something2 of
                  Nothing -> table'
                  Just some2 -> checkSimpleStm table' some2
  in
    if t1 == TypeBool
    then
      checkStm table'' stm
    else
      error "Type error: condition must be bool"

-- Complete: Look for the return type of function
checkStm table (ReturnStatement something) =
  let t1 = case something of
             Just e1 -> checkExpr table e1
             Nothing -> TypeVoid
  in
    True


checkSimpleStm :: SymTable -> Simple -> SymTable
checkSimpleStm table (AssignOperation (Assign id e1)) =
  case Map.lookup id table of
    Nothing -> error "undeclared var"
    Just t1 -> let t2 = checkExpr table e1
               in
                 if t1 == t2
                 then
                   table
                 else
                   error "type error in assignment"

checkSimpleStm table (VariableDeclaration t1 id e1) =
  case e1 of
    Nothing -> Map.insert id t1 table
    Just e1 -> checkSimpleStm (Map.insert id t1 table) (AssignOperation (Assign id e1))

-- In c0 you can just have an expression by itself, BUT you sill need to check
-- if its well constructed.
-- Improve this "function" looks weird
checkSimpleStm table (Expression e1) =
  case checkExpr table e1 of
    _ -> table
