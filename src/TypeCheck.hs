module TypeCheck where

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

-- Checks Functions definitions/declarations
checkFunc :: SymTable -> Definitions -> SymTable
checkFunc table (FuncDec t id args) =
  case Map.lookup id table of
    Nothing -> Map.insert id t table
    _ -> error "Function already declared/defined"

checkFunc table (FuncDef t id args stm) =
  checkStm (checkFuncArgs (Map.insert id t table) args) (MultipleStatements stm)

checkFuncArgs :: SymTable -> [Parameters] -> SymTable
checkFuncArgs table [] = table
checkFuncArgs table ((DefParam t id):xs) = checkFuncArgs (Map.insert id t table) xs


-- Check program
checkProgram :: AST -> SymTable
checkProgram (Program def) = auxCheckProgram Map.empty def

{- Não tenho muito a vontade com haskell, mas para apanhar o erro tenho que fazer alguma
   operação sobre a tabela table', a menos que -}
auxCheckProgram :: SymTable -> [Definitions] -> SymTable
auxCheckProgram table [] = table
auxCheckProgram table (x:xs) =
  auxCheckProgram (checkFunc table x) xs
