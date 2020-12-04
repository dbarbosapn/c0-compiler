module MiddleCode where

import Control.Monad.State
import AST
import TypeCheck

import           Data.Map(Map)
import qualified Data.Map as Map

type Count = (Int, Int)
type Table = Map String String
type TableCount = (Table, Count)

type Temp = String
type Label = String

-- No need to separate arithmetic and relational operations, because
-- type check already assures that everything is semantically correct
data Op = Re -- ==
        | Rne -- !=
        | Rg -- >
        | Rge -- >=
        | Rl -- <
        | Rle -- <=
        | Aa -- +
        | As -- -
        | Am --- *
        | Ad -- /
        | Amod -- Guess!
        deriving(Eq, Show)

data Instr = MOVE Temp Temp -- temp1 = temp2
           | MOVEI Temp Int -- temp1 = numero
           | OPE Op Temp Temp Temp -- temp1 = temp2 op temp3
           | LABEL Label
           | JUMP Label
           | COND Temp Op Temp Label Label
           deriving(Eq, Show)

newTemp :: State TableCount Temp
newTemp = do (table, (temps, labels)) <- get
             put (table, (temps+1, labels))
             return ("t" ++ show temps)

newLabel :: State TableCount Label
newLabel = do (table, (temps, labels)) <- get
              put (table, (temps, labels+1))
              return ("L" ++ show labels)

newVar :: String -> State TableCount Temp
newVar key = do t1 <- newTemp
                (table, count) <- get
                table' <- return $ Map.insert key t1 table
                put (table', count)
                return t1

restoreTable :: Table -> State TableCount Table
restoreTable table = do (table', count) <- get
                        put (table, count)
                        return table

lookupTable :: String -> State TableCount Temp
lookupTable id = do (table, count) <- get
                    case Map.lookup id table of
                      Nothing -> return (error "WTF")
                      Just something -> return something

getTable :: State TableCount Table
getTable = do (table, count) <- get
              return table

-- Expressions
transExpr :: Expression-> Temp -> State TableCount [Instr]
transExpr (IntValue n) dest =
  return [MOVEI dest n]

transExpr (Id id) dest =
  do t1 <- lookupTable id
     return [MOVE dest t1]

-- Monads are weird!
transExpr (BinaryOperation (ArithmeticOperation something)) dest =
  do (op, e1, e2) <- case something of
                       (Add e1 e2) -> return (Aa, e1, e2)
                       (Subtract e1 e2) -> return (As, e1, e2)
                       (Multiply e1 e2) -> return (Am, e1, e2)
                       (Modulo e1 e2) -> return (Amod, e1, e2)
     t1 <- newTemp
     t2 <- newTemp
     code1 <- transExpr e1 t1
     code2 <- transExpr e2 t2
     return (code1 ++ code2 ++ [OPE op dest t1 t2])

-- Statements
transStm :: Statement -> State TableCount [Instr]
transStm (Simple (Expression something)) =
  do t1 <- newTemp
     transExpr something t1

-- No need to check for undeclared variables or corrected types.
-- Typecheck does that!
transStm (Simple (AssignOperation (Assign id e1))) =
  do t1 <- lookupTable id
     transExpr e1 t1

transStm (Simple (VariableDeclaration _ id something)) =
  do t1 <- newVar id
     case something of
       Nothing -> return []
       Just e1 -> transExpr e1 t1

transStm (MultipleStatements []) =
  return []
transStm (MultipleStatements (x:xs)) =
  do code1 <- transStm x
     code2 <- transStm (MultipleStatements xs)
     return (code1 ++ code2)

transStm (WhileStatement e1 stm1) =
  do label1 <- newLabel
     label2 <- newLabel
     label3 <- newLabel
     code1 <- transCond e1 label2 label3
     code2 <- transStm stm1
     return ([LABEL label1] ++ code1 ++ [LABEL label2] ++ code2 ++ [JUMP label1, LABEL label3])

transCond :: Expression -> Label -> Label -> State TableCount [Instr]
transCond (BinaryOperation (RelationalOperation something)) l1 l2 =
  do (op, e1, e2) <- case something of
                       Equals e1 e2 -> return (Re, e1, e2)
                       IsNotEqual e1 e2 -> return (Rne, e1, e2)
                       IsMore e1 e2 -> return (Rg, e1, e2)
                       IsMoreOrEqual  e1 e2 -> return (Rge, e1, e2)
                       IsLess e1 e2 -> return (Rl, e1, e2)
                       IsLessOrEqual e1 e2 -> return (Rle, e1, e2)
     t1 <- newTemp
     t2 <- newTemp
     code1 <- transExpr e1 t1
     code2 <- transExpr e2 t2
     return (code1 ++ code2 ++ [COND t1 op t2 l1 l2])
