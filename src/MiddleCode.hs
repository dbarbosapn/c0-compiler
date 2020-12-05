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

newTemp :: State TableCount Temp
newTemp = do
  (table, (temps, labels)) <- get
  put (table, (temps + 1, labels))
  return ("t" ++ show temps)

resetTemps :: State TableCount Int
resetTemps = do
  (table, (temps, labels)) <- get
  put (table, (0, labels))
  return temps

newLabel :: Maybe String -> State TableCount Label
newLabel something = do
  (table, (temps, labels)) <- get
  put (table, (temps, labels + 1))
  case something of
    Nothing -> return ("L" ++ show labels)
    Just str -> return str

newVar :: String -> State TableCount Temp
newVar key = do
  t1 <- newTemp
  (table, count) <- get
  table' <- return $ Map.insert key t1 table
  put (table', count)
  return t1

restoreTable :: Table -> State TableCount Table
restoreTable table = do
  (table', count) <- get
  put (table, count)
  return table

lookupTable :: String -> State TableCount Temp
lookupTable id = do
  (table, count) <- get
  case Map.lookup id table of
    Nothing -> return (error "Could not find id in table")
    Just something -> return something

getTable :: State TableCount Table
getTable = do
  (table, count) <- get
  return table

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
    label1 <- newLabel Nothing
    label2 <- newLabel Nothing
    label3 <- newLabel Nothing
    code1 <- transCond e1 label2 label3
    code2 <- transStm stm1
    return ([LABEL label1] ++ code1 ++ [LABEL label2] ++ code2 ++ [JUMP label1, LABEL label3])

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

translateProgram :: AST -> [Instr]
translateProgram (Program definitions) = evalState (transDefs definitions) (Map.empty, (0, 0))

transDefs :: [Definition] -> State TableCount [Instr]
transDefs [] = return []
transDefs (x : xs) = do
  code1 <- transDef x
  code2 <- transDefs xs
  return (code1 ++ code2)

transDef :: Definition -> State TableCount [Instr]
transDef (FuncDef t id params stm) = do
  label <- newLabel (Just id)
  table <- getTable
  resetTemps
  transDefParams params
  code <- transStm stm
  return ((LABEL label) : code)

transDefParams :: [Parameter] -> State TableCount [Temp]
transDefParams [] = return []
transDefParams (x : xs) = do
  t <- newTemp
  rest <- transDefParams xs
  return (t : rest)