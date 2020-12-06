module MiddleCode where

import AST
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import TypeCheck

type Count = (Int, Int)

type Table = Map String String

type TableCount = (Table, Count)

type Temp = String

type Label = String

-- No need to separate arithmetic and relational operations, because
-- type check already assures that everything is semantically correct
data Op
  = R_EQ -- ==
  | R_NEQ -- !=
  | R_GREATER -- >
  | R_GREATEREQ -- >=
  | R_LESS -- <
  | R_LESSEQ -- <=
  | A_ADD -- +
  | A_SUB -- -
  | A_MULT --- *
  | A_DIV -- /
  | A_MOD -- Guess!
  deriving (Eq, Show)

data Instr
  = MOVE Temp Temp -- temp1 = temp2
  | MOVEI Temp Int -- temp1 = num
  | OP Op Temp Temp Temp -- temp1 = temp2 op temp3
  | OPI Op Temp Temp Int -- temp1 = temp2 op num
  | LABEL Label
  | JUMP Label
  | COND Temp Op Temp Label Label
  | RETURN_NOTHING
  | RETURN Temp
  deriving (Eq, Show)

type Args = [Temp]
type FuncId = String
data Function = Def FuncId Args [Instr]
              deriving (Eq, Show)

-- New State Stuff
newTemp :: State TableCount Temp
newTemp = do
  (table, (temps, labels)) <- get
  put (table, (temps + 1, labels))
  return ("t" ++ show temps)

newLabel :: State TableCount Label
newLabel = do
  (table, (temps, labels)) <- get
  put (table, (temps, labels + 1))
  return ("L" ++ show labels)

newVar :: String -> State TableCount Temp
newVar key = do
  t1 <- newTemp
  (table, count) <- get
  table' <- return $ Map.insert key t1 table
  put (table', count)
  return t1

-- Restore State
restoreTempCount :: Int -> State TableCount Int
restoreTempCount n =
  do (table, (temps, labels)) <- get
     put (table, (n, labels))
     return n

restoreTable :: Table -> State TableCount Table
restoreTable table = do
  (table', count) <- get
  put (table, count)
  return table

-- Look up in table
lookupTable :: String -> State TableCount Temp
lookupTable id = do
  (table, count) <- get
  case Map.lookup id table of
    Nothing -> return (error "Could not find id in table")
    Just something -> return something

-- Get State Info
getTable :: State TableCount Table
getTable = do
  (table, count) <- get
  return table

getTempCount :: State TableCount Int
getTempCount =
  do (_, (temps, _)) <- get
     return temps

-- Expressions
transExpr :: Expression -> Temp -> State TableCount [Instr]
transExpr (IntValue n) dest =
  return [MOVEI dest n]
transExpr (Id id) dest =
  do
    t1 <- lookupTable id
    return [MOVE dest t1]

transExpr (BinaryOperation (ArithmeticOperation something)) dest =
  do
    (op, e1, e2) <- case something of
      (Add e1 e2) -> return (A_ADD, e1, e2)
      (Subtract e1 e2) -> return (A_SUB, e1, e2)
      (Multiply e1 e2) -> return (A_MULT, e1, e2)
      (Modulo e1 e2) -> return (A_MOD, e1, e2)
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExpr e1 t1
    code2 <- transExpr e2 t2
    return (code1 ++ code2 ++ [OP op dest t1 t2])

-- Statements
transStm :: Statement -> State TableCount [Instr]
transStm (Simple (Expression something)) =
  do
    t1 <- newTemp
    transExpr something t1

-- No need to check for undeclared variables or correct types.
-- Typecheck does that!
transStm (Simple (AssignOperation (Assign id e1))) =
  do
    t1 <- lookupTable id
    transExpr e1 t1
transStm (Simple (VariableDeclaration _ id something)) =
  do
    t1 <- newVar id
    case something of
      Nothing -> return []
      Just e1 -> transExpr e1 t1
transStm (MultipleStatements []) =
  return []
transStm (MultipleStatements (x : xs)) =
  do
    code1 <- transStm x
    code2 <- transStm (MultipleStatements xs)
    return (code1 ++ code2)
transStm (WhileStatement e1 stm1) =
  do
    label1 <- newLabel
    label2 <- newLabel
    label3 <- newLabel
    code1 <- transCond e1 label2 label3
    code2 <- transStm stm1
    return ([LABEL label1] ++ code1 ++ [LABEL label2] ++ code2 ++ [JUMP label1, LABEL label3])

transStm (ReturnStatement Nothing) =
  return [RETURN_NOTHING]
transStm (ReturnStatement (Just expr)) =
  do t1 <- newTemp
     code1 <- transExpr expr t1
     return (code1 ++ [RETURN t1])

transCond :: Expression -> Label -> Label -> State TableCount [Instr]
transCond (BinaryOperation (RelationalOperation something)) l1 l2 =
  do
    let (op, e1, e2) = case something of
          Equals e1 e2 -> (R_EQ, e1, e2)
          IsNotEqual e1 e2 -> (R_NEQ, e1, e2)
          IsMore e1 e2 -> (R_GREATER, e1, e2)
          IsMoreOrEqual e1 e2 -> (R_GREATEREQ, e1, e2)
          IsLess e1 e2 -> (R_LESS, e1, e2)
          IsLessOrEqual e1 e2 -> (R_LESSEQ, e1, e2)
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExpr e1 t1
    code2 <- transExpr e2 t2
    return (code1 ++ code2 ++ [COND t1 op t2 l1 l2])

transArgs :: [Parameter] -> State TableCount [Temp]
transArgs [] = return []
transArgs ((DefParam _ var):xs) =
  do t1 <- newVar var
     rest <- transArgs xs
     return (t1:rest)

transFunction :: Definition -> State TableCount Function
transFunction (FuncDef _ id args stm) =
  do args' <- transArgs args
     code1 <- transStm stm
     return (Def id args' code1)

transDefs :: [Definition] -> State TableCount [Function]
transDefs [] = return []
transDefs (x : xs) =
  do table <- getTable
     tempCount <- getTempCount
     elem <- transFunction x
     -- Before we go to the next definition we need to restore the table and temp count (They are local)
     -- Instead of simply putting everything at "zero", it's more "powerfull" to get the previous struff
     restoreTable table
     restoreTempCount tempCount
     rest <- transDefs xs
     return (elem:rest)

transProgram :: AST -> [Function]
transProgram (Program []) = []
transProgram (Program x) = evalState (transDefs x) (Map.empty, (0,0))
