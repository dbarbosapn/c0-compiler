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
checkStm :: SymTable -> Statement -> SymTable
-- Simple
checkStm table (Simple something) = checkSimpleStm table something

--Ifs
checkStm table (IfStatement expr stm) =
  case checkExpr table expr of
    TypeBool -> checkStm table stm
    _ -> error "IfStatement: Condition must be bool"

checkStm table (IfElseStatement expr stm1 stm2) =
  checkStm (checkStm table (IfStatement expr stm1)) stm2

-- While
checkStm table (WhileStatement expr stm) =
  case checkExpr table expr of
    TypeBool -> checkStm table stm
    _ -> error "WhileStatement: Condition must be bool"

-- For
checkStm table (ForStatement (stm1, expr, stm2) stm3) =
  case checkExpr table expr of
    TypeBool -> let table' = case stm1 of
                               Nothing -> table
                               Just something -> checkSimpleStm table something
                    table'' = case stm2 of
                                Nothing -> table'
                                Just something -> checkSimpleStm table' something
                in
                  table''
    _-> error "ForStatement: Condition must be bool"

-- Multiple statements
checkStm table (MultipleStatements [x]) =
  checkStm table x
checkStm table (MultipleStatements (x:xs)) =
  checkStm (checkStm table x) (MultipleStatements xs)

-- Simple Checks. Contains Variable Declaration (Important)
checkSimpleStm :: SymTable -> Simple -> SymTable
checkSimpleStm table (AssignOperation (Assign id expr)) =
  case Map.lookup id table of
    Nothing -> error "undeclared var"
    Just t -> let t1 = checkExpr table expr
              in
                if t == t1 then
                  table
                else
                  error "type error in assignment"

checkSimpleStm table (VariableDeclaration t id expr) =
  case Map.lookup id table of
    Just t1 -> error "Variable already declared"
    Nothing -> case expr of
                 Nothing -> Map.insert id t table
                 Just expr1 -> checkSimpleStm (Map.insert id t table) (AssignOperation (Assign id expr1))

-- In c0 we can have a expression alone, but it still needs to be checked
checkSimpleStm table (Expression expr) =
  case checkExpr table expr of
    _ -> table

-- -- Statements
-- checkStm :: SymTable -> Statement -> Maybe SymTable
-- checkStm table (Simple something) =
--   case (checkSimpleStm table something) of
--     Nothing -> Nothing
--     Just table' -> Just table'

-- checkStm table (IfStatement e1 stm1) =
--   case (checkExpr table e1) of
--     TypeBool -> checkStm table stm1
--     _ ->  error "type error: condition must be bool"

-- checkStm table (IfElseStatement e1 stm1 stm2) =
--   case (checkStm table (IfStatement e1 stm1)) of
--     Just table' -> (checkStm table' stm2)
--     Nothing -> Nothing

-- checkStm table (WhileStatement e1 stm1) =
--   case (checkExpr table e1) of
--     TypeBool -> checkStm table stm1
--     _ -> error "type error: condition must be bool"

-- checkStm table (ForStatement (something1, e1, something2) stm) =
--   let t1 = checkExpr table e1
--       table' = case something1 of
--                  Nothing -> table
--                  Just some1 -> checkSimpleStm table some1
--       table'' = case something2 of
--                   Nothing -> table'
--                   Just some2 -> checkSimpleStm table' some2
--   in
--     if t1 == TypeBool
--     then
--       checkStm table'' stm
--     else
--       error "Type error: condition must be bool"


-- checkSimpleStm :: SymTable -> Simple -> Maybe SymTable
-- checkSimpleStm table (AssignOperation (Assign id e1)) =
--   case Map.lookup id table of
--     Nothing -> error "undeclared var"
--     Just t1 -> let t2 = checkExpr table e1
--                in
--                  if t1 == t2
--                  then
--                    table
--                  else
--                    error "type error in assignment"

-- checkSimpleStm table (VariableDeclaration t1 id e1) =
--   case e1 of
--     Nothing -> Map.insert id t1 table
--     Just e1 -> checkSimpleStm (Map.insert id t1 table) (AssignOperation (Assign id e1))

-- -- In c0 you can just have an expression by itself, BUT you sill need to check
-- -- if its well constructed.
-- -- Improve this "function" looks weird
-- checkSimpleStm table (Expression e1) =
--   case checkExpr table e1 of
--     _ -> table
