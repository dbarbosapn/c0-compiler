module TypeCheck where

import           AST

import           Data.Map(Map)
import qualified Data.Map as Map

{- TODO:  Check if return type matches function type
          Check if function call arguments match function definition arguments
          Add readable messages when typecheck errors are found
-}

-- Symbol Table
type SymTable = Map String Type


-- Expressions
checkExpr :: Maybe SymTable -> Expression -> Maybe Type
checkExpr Nothing _ = Nothing
-- Basic types
checkExpr _ (IntValue _) = Just TypeInt
checkExpr _ (BoolValue _) = Just TypeBool
checkExpr _ (StringValue _) = Just TypeChar
checkExpr _ (CharValue _) = Just TypeChar

checkExpr (Just table) (Id id) = Map.lookup id table

checkExpr (Just table) (FunctionCall id expr) = Map.lookup id table

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
    if t1' == Just TypeInt && t2' == Just TypeInt then
      Just TypeInt
    else
      Nothing

-- We are only comparing numbers. Not strings.
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
    if t1' == Just TypeInt && t2' == Just TypeInt then
      Just TypeBool
    else
      Nothing

-- Statements
checkStm :: Maybe SymTable -> Statement -> Maybe SymTable
checkStm Nothing _ = Nothing
-- Simple
checkStm table (Simple something) = checkSimpleStm table something

--Ifs
checkStm table (IfStatement expr stm) =
  case checkExpr table expr of
    Just TypeBool -> checkStm table stm
    _ -> Nothing

checkStm table (IfElseStatement expr stm1 stm2) =
  checkStm (checkStm table (IfStatement expr stm1)) stm2

-- While
checkStm table (WhileStatement expr stm) =
  case checkExpr table expr of
    Just TypeBool -> checkStm table stm
    _ -> Nothing

-- For
checkStm table (ForStatement (stm1, expr, stm2) stm3) =
  case checkExpr table expr of
    Just TypeBool -> let table' = case stm1 of
                                    Nothing -> table
                                    Just something -> checkSimpleStm table something
                         table'' = case stm2 of
                                     Nothing -> table'
                                     Just something -> checkSimpleStm table' something
                     in
                       table''
    _-> Nothing

-- Multiple statements
checkStm table (MultipleStatements []) = table
checkStm table (MultipleStatements (x:xs)) =
  checkStm (checkStm table x) (MultipleStatements xs)


checkStm table _ = table

-- Simple Checks. Contains Variable Declaration (Important)
checkSimpleStm :: Maybe SymTable -> Simple -> Maybe SymTable
checkSimpleStm Nothing _ = Nothing

checkSimpleStm (Just table) (AssignOperation (Assign id expr)) =
  case Map.lookup id table of
    Nothing -> Nothing
    Just t -> let t1 = checkExpr (Just table) expr
              in
                if (Just t) == t1 then
                  Just table
                else
                  Nothing

checkSimpleStm (Just table) (VariableDeclaration t id expr) =
  case Map.lookup id table of
    Just t1 -> Nothing
    Nothing -> case expr of
                 Nothing -> Just (Map.insert id t table)
                 Just expr1 -> checkSimpleStm (Just (Map.insert id t table)) (AssignOperation (Assign id expr1))

-- In c0 we can have an expression by itself, but it still needs to be checked
checkSimpleStm table (Expression expr) =
  case checkExpr table expr of
    Nothing -> Nothing
    Just _ -> table

-- Checks Functions definitions/declarations
checkFunc :: Maybe SymTable -> Definitions -> Maybe SymTable
checkFunc Nothing _ = Nothing

checkFunc (Just table) (FuncDec t id args) =
  case Map.lookup id table of
    Nothing -> Just (Map.insert id t table)
    _ -> Nothing

checkFunc (Just table) (FuncDef t id args stm) =
  checkStm (checkFuncArgs (Just (Map.insert id t table)) args) stm

checkFuncArgs :: Maybe SymTable -> [Parameters] -> Maybe SymTable
checkFuncArgs Nothing _ = Nothing

checkFuncArgs table [] = table
checkFuncArgs (Just table) ((DefParam t id):xs) = checkFuncArgs (Just (Map.insert id t table)) xs


-- Create the base symtable
baseTable :: Maybe SymTable
baseTable = Just (Map.insert "scan_int" TypeInt (Map.insert "print_int" TypeVoid (Map.insert "print_str" TypeVoid Map.empty)))

-- Check program
checkProgram :: AST -> Bool
checkProgram (Program def) =
  case (auxCheckProgram (baseTable) def) of
    Nothing -> False
    _ -> True

auxCheckProgram :: Maybe SymTable -> [Definitions] -> Maybe SymTable
auxCheckProgram Nothing _ = Nothing

auxCheckProgram table [] = table
auxCheckProgram table (x:xs) =
  let table' = case x of
                 FuncDec t id args -> checkFunc table x
                 FuncDef t id args stm -> checkFunc table (FuncDec t id args)
  in
    case x of
      FuncDec _ _ _ -> auxCheckProgram table' xs
      _ -> case checkFunc table' x of
             Nothing -> Nothing
             _ -> auxCheckProgram table' xs
