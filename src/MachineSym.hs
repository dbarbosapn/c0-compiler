module MachineSym where

{- To do: Add immediate, mips dosent have subi and muli so we need to do that in
          typecheck
          Move S, needs to allocate an array -}

import MiddleCode
import Control.Monad.State
import Data.List

type VarCount = (Int, Int, Int) --- $a $t $s
type MipsProgram = [String] -- In reverse
type Info = (MipsProgram, VarCount)

-- Important Note: Because i only look locally, don't actually know what needs to
-- be saved. So we have three options. For each time we use a saved reg we save it
-- in the stack, OR we keep track of the number of variables used in case we call
-- something, OR we look head. Both have their set backs, but i thick the easier one
-- to implement would be to keep track of the variables


-- generateMips :: [Function] -> IO()
-- generateMips something =
--   case something of
--     [] -> putStrLn ""
--     list -> do decodeFunction $ head list
--                generateMips $ tail list

addInfo :: String -> State Info ()
addInfo str =
  do (info, count) <- get
     put (str:info, count) -- It's more efficient to build it in reverse xD

getProgram :: State Info MipsProgram
getProgram =
  do (prog, count) <- get
     return $ reverse prog

restoreCount :: VarCount -> State Info ()
restoreCount count =
  do (prog, _) <- get
     put (prog, count)

getCount :: State Info VarCount
getCount =
  do (_, count) <- get
     return count

generateMips :: [Function] -> IO ()
generateMips something =
  putStrLn $ intercalate "\n" $ evalState (decodeProgram something) ([], (0,0,0))

decodeProgram :: [Function] -> State Info MipsProgram
decodeProgram [] = getProgram

decodeProgram (x:xs) =
  do decodeFunction x
     decodeProgram xs

-- Needs more work
decodeFunction :: Function -> State Info ()
decodeFunction (Def id args inst) =
  do addInfo $ "_" ++ id ++ "_:"
     decodeInstr inst

-- Needs more work
decodeReturn :: Instr -> State Info ()
decodeReturn something =
  do case something of
       RET -> addInfo $ "\tReturn"
       RETVAL t -> addInfo $ "\tReturn " ++ t
       _ -> addInfo $ "Something wrong"

-- Need more work
decodeFuncCall :: Instr -> State Info ()
decodeFuncCall (CALL id args) =
  do addInfo $ "\t" ++ "call " ++ id

decodeInstr :: [Instr] -> State Info ()
decodeInstr [] = return ()
decodeInstr (x:xs) =
  do case x of
       OP _ _ _ _ -> decodeInstArithmetic x
       MOVE t1 t2 -> do addInfo $ "\t" ++ "move " ++ t1 ++ ", " ++ t2
       MOVEI t1 n -> do addInfo $ "\t" ++ "li " ++ t1 ++ ", " ++ (show n)
       CALL _ _ -> decodeFuncCall x
       LABEL l1 -> addInfo $ l1 ++ ":"
       COND _ _ _ _ _ -> decodeWhile x
       COND_ZERO _ _ _ -> decodeWhile x
       JUMP l -> addInfo $ "\t" ++ "jump " ++ l
       _ -> decodeReturn x
     decodeInstr xs

decodeInstArithmetic :: Instr -> State Info ()
decodeInstArithmetic (OP op t1 t2 t3) =
  do case op of
       A_ADD -> addInfo $ "\t" ++ "add " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3
       A_MULT -> addInfo $ "\t" ++ "mul " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3
       A_SUB -> addInfo $ "\t" ++ "sub " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3
       _ -> do addInfo $ "\t" ++ "div" ++ t2 ++ ", " ++ t3
               case op of
                 A_DIV -> addInfo $ "\t" ++ "mflo " ++ t1
                 A_MOD -> addInfo $ "\t" ++ "mfhi " ++ t1

-- Our middle code does alot, so the while statement its simple to do here
decodeWhile :: Instr -> State Info ()
decodeWhile (COND_ZERO t1 l1 l2) =
  do addInfo $ "\t" ++ "beq " ++ t1 ++ ", " ++ "$zero" ++ ", " ++ l2

decodeWhile (COND t1 op t2 l1 l2) =
  do let str = case op of
                 R_EQ -> "beq "
                 R_NEQ -> "bne "
                 R_GREATER -> "bgt "
                 R_GREATEREQ -> "bge "
                 R_LESS -> "blt "
                 R_LESSEQ -> "ble "
     addInfo $ "\t" ++ str ++ t1 ++ ", " ++ t2 ++ ", " ++ l2
