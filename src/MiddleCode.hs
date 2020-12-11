module MiddleCode where

import AST
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import TypeCheck

type Count = (Int, Int)

type Table = Map String String

type TableCount = (Table, Count)

type Temp = String

zero = "zero"

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

type Args = [Temp]
type FuncId = String
data Function = Def FuncId Args [Instr]
              deriving (Eq, Show)

data Instr
  = MOVE Temp Temp -- temp1 = temp2
  | MOVEI Temp Int -- temp1 = num
  | MOVES Temp String -- temp1 = str
  | OP Op Temp Temp Temp -- temp1 = temp2 op temp3
  | OPI Op Temp Temp Int -- temp1 = temp2 op num
  | LABEL Label
  | CALL FuncId Args
  | JUMP Label
  | COND Temp Op Temp Label Label
  | RET
  | RETVAL Temp
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
transExpr (BoolValue b) dest = if b 
  then return [MOVEI dest 1] 
  else return [MOVEI dest 0]
transExpr (StringValue s) dest =
  return [MOVES dest s]
transExpr (CharValue n) dest =
  return [MOVEI dest (ord n)]

transExpr (Id id) dest =
  do
    t1 <- lookupTable id
    return [MOVE dest t1]

transExpr (BinaryOperation (ArithmeticOperation something)) dest =
  do
    (op, e1, e2) <- case something of
      (Add e1 e2) -> return (A_ADD, e1, e2)
      (Subtract e1 e2) -> return (A_SUB, e1, e2)
      (Divide e1 e2) -> return (A_DIV, e1, e2)
      (Multiply e1 e2) -> return (A_MULT, e1, e2)
      (Modulo e1 e2) -> return (A_MOD, e1, e2)
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExpr e1 t1
    code2 <- transExpr e2 t2
    return (code1 ++ code2 ++ [OP op dest t1 t2])

transExpr (BinaryOperation (RelationalOperation something)) dest =
  do
    l1 <- newLabel
    l2 <- newLabel
    code1 <- transCond (BinaryOperation (RelationalOperation something)) l1 l2
    return (code1 ++ [LABEL l1, MOVEI dest 1, LABEL l2, MOVEI dest 0])

transExpr (FunctionCall id params) dest =
  do
    (insts, tmps) <- transCallParams params
    return (insts ++ [CALL id tmps])

transCallParams :: [Expression] -> State TableCount ([Instr], [Temp])
transCallParams [] = return ([], [])
transCallParams (x:xs) =
  do
    t1 <- newTemp
    code1 <- transExpr x t1
    (insts, tmps) <- transCallParams xs
    return (code1 ++ insts, t1:tmps)

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

transStm (IfStatement e1 stm1) =
  do
    l1 <- newLabel
    l2 <- newLabel
    code1 <- transCond e1 l1 l2
    code2 <- transStm stm1
    return (code1 ++ [LABEL l1] ++ code2 ++ [LABEL l2])

transStm (IfElseStatement e1 stm1 stm2) =
  do
    l1 <- newLabel
    l2 <- newLabel
    l3 <- newLabel
    code1 <- transCond e1 l1 l2
    code2 <- transStm stm1
    code3 <- transStm stm2
    return (code1 ++ [LABEL l1] ++ code2 ++ [JUMP l3, LABEL l2] ++ code3 ++ [LABEL l3])

transStm (ForStatement (something, expr, Nothing) stm) =
  do code1 <- case something of
             Nothing -> return []
             Just e1 -> transStm (Simple e1)
     code2 <- transStm (WhileStatement expr stm)
     return (code1 ++ code2)

transStm (ForStatement (something1, expr, Just something2) stm) =
  do code1 <- case something1 of
             Nothing -> return []
             Just e1 -> transStm (Simple e1)
     l1 <- newLabel
     l2 <- newLabel
     l3 <- newLabel
     code2 <- transCond expr l2 l3
     code3 <- transStm stm
     code4 <- transStm (Simple something2)
     return (code1 ++ [LABEL l1] ++ code2 ++ [LABEL l2] ++ code3 ++ code4 ++ [JUMP l1, LABEL l3])

transStm (ReturnStatement Nothing) =
  return [RET]
transStm (ReturnStatement (Just expr)) =
  do t1 <- newTemp
     code1 <- transExpr expr t1
     return (code1 ++ [RETVAL t1])

transCond :: Expression -> Label -> Label -> State TableCount [Instr]
transCond (BinaryOperation (RelationalOperation something)) l1 l2 =
  do
    (op, e1, e2) <- case something of
      (Equals e1 e2) -> return (R_EQ, e1, e2)
      (IsLessOrEqual e1 e2) -> return (R_LESSEQ, e1, e2)
      (IsMoreOrEqual e1 e2) -> return (R_GREATEREQ, e1, e2)
      (IsNotEqual e1 e2) -> return (R_NEQ, e1, e2)
      (IsLess e1 e2) -> return (R_LESS, e1, e2)
      (IsMore e1 e2) -> return (R_GREATER, e1, e2)
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExpr e1 t1
    code2 <- transExpr e2 t2
    return (code1 ++ code2 ++ [COND t1 op t2 l1 l2])
transCond e1 l1 l2 =
  do
    t1 <- newTemp
    code1 <- transExpr e1 t1
    return (code1 ++ [COND t1 R_GREATER zero l1 l2])


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
