module MachineSym where

{- To do: Add immediate, mips dosent have subi and muli so we need to do that in typecheck
          Move S, needs to allocate an array
          URGENT: Something wrong when: n = "Function call". We are not saving the result in a variable.
          Middle code SHOULD say something about it, for example SAVED CALL dest, so that we know how
          to proceed here. Then the only thing left would be to pass v0 to the actual variable
          Change "save something" "delete something" to proper MIPS, basically adding to the stack and saving
          and loading followed by taking from the stack
          There is also some mumbo jumbo realated to call, like saving the return address etc..
          -}

import MiddleCode
import Control.Monad.State
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

-- type VarCount = (Int, Int, Int) --- $a $t $s
-- type MipsProgram = [String] -- In reverse
-- type Info = (MipsProgram, VarCount)

type VarTrack = (Set Arg, Set Temp, Set Saved)

-- Monad stuff
getInfo :: State VarTrack VarTrack
getInfo =
  do something <- get
     return something

getInfoSaved :: State VarTrack [Saved]
getInfoSaved =
  do (_, y, _) <- get
     return $ Set.toList y

resetInfo :: State VarTrack ()
resetInfo =
  do put (Set.empty, Set.empty, Set.empty)

resetInfoTemp :: State VarTrack ()
resetInfoTemp =
  do (x, y, z) <- get
     put (x, Set.empty, z)

restoreInfo :: VarTrack -> State VarTrack ()
restoreInfo something =
  do put something

addVar :: String -> State VarTrack ()
addVar (w:ws) =
  do (x, y, z) <- getInfo
     case w of
       's' -> put (x, y, Set.insert (w:ws) z)
       't' -> put (x, Set.insert (w:ws) y, z)
       'a' -> put (Set.insert (w:ws) x, y, z)

-- Generation stuff
generateMips :: [Function] -> IO ()
generateMips something =
  putStrLn $ evalState (decodeProgram something) (Set.empty, Set.empty, Set.empty)

decodeProgram :: [Function] -> State VarTrack String
decodeProgram [] = return ""
decodeProgram ((Def id args inst):xs) =
  do resetInfo
     body <- decodeInstr inst
     (x, y, z) <- getInfo
     let list = Set.toList z
     start <- saveInStack list 0
     end <- deleteFromStack list (length list - 1)
     rest <- decodeProgram xs
     return $ "_" ++ id ++ "_:\n" ++ start ++ body ++ end ++ rest

saveInStack :: [String] -> Int -> State VarTrack String
saveInStack [] _ = return ""
saveInStack (x:xs) n =
  do let str = "\tsave " ++ x ++ "\n"
     str' <- saveInStack xs (n+1)
     return (str ++ str')

deleteFromStack :: [String] -> Int -> State VarTrack String
deleteFromStack [] _ = return ""
deleteFromStack (x:xs) n =
  do let str = "\tdelete " ++ x ++ "\n"
     str' <- deleteFromStack xs (n-1)
     return (str ++ str')

decodeInstr :: [Instr] -> State VarTrack String
decodeInstr [] = return ""
decodeInstr ((CALL id args):xs) =
  decodeFuncCall ((CALL id args):xs)

decodeInstr (x:xs) =
  do str <- case x of
              OP _ _ _ _ -> decodeInstArithmetic x
              MOVE _ _ -> decodeMove x
              MOVEI _ _ -> decodeMove x
              LABEL l1 -> return (l1 ++ ":\n")
              COND _ _ _ _ _ -> decodeWhile x
              COND_ZERO _ _ _ -> decodeWhile x
              JUMP l -> return ("\t" ++ "jump " ++ l ++ "\n" )
              _ -> decodeReturn x
     str' <- decodeInstr xs
     return (str ++ str')

decodeInstArithmetic :: Instr -> State VarTrack String
decodeInstArithmetic (OP op t1 t2 t3) =
  do addVar t1
     addVar t2
     addVar t3
     case op of
       A_ADD -> return $ "\t" ++ "add " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3 ++ "\n"
       A_MULT -> return $ "\t" ++ "mul " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3 ++ "\n"
       A_SUB -> return $ "\t" ++ "sub " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3 ++ "\n"
       _ -> do return $ "\t" ++ "div" ++ t2 ++ ", " ++ t3
               case op of
                 A_DIV -> return $ "\t" ++ "mflo " ++ t1 ++ "\n"
                 A_MOD -> return $ "\t" ++ "mfhi " ++ t1 ++ "\n"

decodeMove :: Instr -> State VarTrack String
decodeMove (MOVE t1 t2) =
  do addVar t1
     addVar t2
     return $ "\t" ++ "move " ++ t1 ++ ", " ++ t2 ++ "\n"

decodeMove (MOVEI t n) =
  do addVar t
     return $ "\t" ++ "li " ++ t ++ ", " ++ (show n) ++ "\n"

decodeWhile :: Instr -> State VarTrack String
decodeWhile (COND_ZERO t1 l1 l2) =
  do addVar t1
     return $ "\t" ++ "beq " ++ t1 ++ ", " ++ "$zero" ++ ", " ++ l2 ++ "\n"

decodeWhile (COND t1 op t2 l1 l2) =
  do let str = case op of
                 R_EQ -> "beq "
                 R_NEQ -> "bne "
                 R_GREATER -> "bgt "
                 R_GREATEREQ -> "bge "
                 R_LESS -> "blt "
                 R_LESSEQ -> "ble "
     addVar t1
     addVar t2
     return $ "\t" ++ str ++ t1 ++ ", " ++ t2 ++ ", " ++ l2 ++ "\n"

decodeFuncCall :: [Instr] -> State VarTrack String
decodeFuncCall [] = return ""
decodeFuncCall ((CALL id args):xs) =
  do (x, y, z) <- getInfo
     resetInfoTemp
     body <- decodeInstr xs
     (_, y', _) <- getInfo
     -- Não vale a pena guardar temps que mais a frente são usados que não foram inicializados
     -- antes. Ha espaço para melhorar a implementação
     let list = intersect (Set.toList y) (Set.toList y')
     restoreInfo (x, y, z)
     start <- saveInStack list 0
     end <- deleteFromStack list (length list -1)
     return (start ++ "\t" ++ "call " ++ id ++ "\n" ++ body ++ end)

decodeReturn :: Instr -> State VarTrack String
decodeReturn something =
  do (x, y, z) <- getInfo
     let list = Set.toList z
     start <- deleteFromStack list (length list -1)
     let end = case something of
                 RET -> "\tReturn\n"
                 RETVAL t -> "\tReturn " ++ t ++ "\n"
                 _ -> "Something wrong probably string related" ++ "\n"
     return (start ++ end)
