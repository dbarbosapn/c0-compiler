module MachineSym where

{- TODO:  Improvements (Still works without):
          There's no need for an extra "jr $ra" if we have a return at the end
          To follow the mips convention, we should not do operations directly with $a0 etc.
          There's no need for a MOVE_RET if we're doing a call without an assignment. (MiddleCode)

          Must:
          Add immediate, mips dosent have subi and muli so we need to do that in typecheck
          Move S, needs to allocate an array
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

-- Jumps to main and then exits the program
prefixCode :: String
prefixCode = "\tjal _main_\n\tli $v0, 10\n\tsyscall\n"

suffixCode :: String
suffixCode = "print_int:\n\tli $2, 1\n\tsyscall\n\tjr $ra\n"

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
generateMips :: [Function] -> String
generateMips something = prefixCode ++ (evalState (decodeProgram something) (Set.empty, Set.empty, Set.empty)) ++ suffixCode

decodeProgram :: [Function] -> State VarTrack String
decodeProgram [] = return ""
decodeProgram ((Def id args inst):xs) =
  do resetInfo
     body <- decodeInstr inst
     rest <- decodeProgram xs
     return $ "_" ++ id ++ "_:\n" ++ body ++ "\tjr $ra\n" ++ rest

saveInStack :: [String] -> State VarTrack String
saveInStack list = 
  do
    let n = length list
    let allocate = "\taddiu $sp, $sp, -" ++ show ((n+1) * 4) ++ "\n"
    let str = "\tsw $ra, " ++ show (n*4) ++ "($sp)\n"
    str' <- saveInStackRec list (n-1)
    return $ allocate ++ str ++ str'

saveInStackRec :: [String] -> Int -> State VarTrack String
saveInStackRec [] _ = return ""
saveInStackRec (x:xs) n =
  do let str = "\tsw $" ++ x ++ ", " ++ show (n*4) ++ "($sp)\n"
     str' <- saveInStackRec xs (n-1)
     return (str ++ str')

deleteFromStack :: [String] -> State VarTrack String
deleteFromStack l = 
  do
    let n = length l
    let str1 = "\tlw $ra, " ++ show (n*4) ++ "($sp)\n"
    str2 <- loadFromStack l (n-1)
    let str3 = "\taddiu $sp, $sp, " ++ show ((n+1) * 4) ++ "\n"
    return $ (str1 ++ str2 ++ str3)

loadFromStack :: [String] -> Int -> State VarTrack String
loadFromStack [] _ = return ""
loadFromStack (x:xs) n =
  do
    let str = "\tlw $" ++ x ++ ", " ++ show (n*4) ++ "($sp)\n"
    str' <- saveInStackRec xs (n-1)
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
              MOVE_RET _ -> decodeMove x
              LABEL l1 -> return (l1 ++ ":\n")
              COND _ _ _ _ _ -> decodeCond x
              COND_ZERO _ _ _ -> decodeCond x
              JUMP l -> return ("\t" ++ "j " ++ l ++ "\n" )
              _ -> decodeReturn x
     str' <- decodeInstr xs
     return (str ++ str')

decodeInstArithmetic :: Instr -> State VarTrack String
decodeInstArithmetic (OP op t1 t2 t3) =
  do addVar t1
     addVar t2
     addVar t3
     case op of
       A_ADD -> return $ "\t" ++ "add $" ++ t1 ++ ", $" ++ t2 ++ ", $" ++ t3 ++ "\n"
       A_MULT -> return $ "\t" ++ "mul $" ++ t1 ++ ", $" ++ t2 ++ ", $" ++ t3 ++ "\n"
       A_SUB -> return $ "\t" ++ "sub $" ++ t1 ++ ", $" ++ t2 ++ ", $" ++ t3 ++ "\n"
       _ -> do return $ "\t" ++ "div $" ++ t2 ++ ", $" ++ t3
               case op of
                 A_DIV -> return $ "\t" ++ "mflo " ++ t1 ++ "\n"
                 A_MOD -> return $ "\t" ++ "mfhi " ++ t1 ++ "\n"

decodeMove :: Instr -> State VarTrack String
decodeMove (MOVE t1 t2) =
  do addVar t1
     addVar t2
     return $ "\t" ++ "move $" ++ t1 ++ ", $" ++ t2 ++ "\n"

decodeMove (MOVEI t n) =
  do addVar t
     return $ "\t" ++ "li $" ++ t ++ ", " ++ (show n) ++ "\n"

decodeMove (MOVE_RET t) =
  do addVar t
     return $ "\t" ++ "move $" ++ t ++ ", $v0" ++ "\n"

decodeCond :: Instr -> State VarTrack String
decodeCond (COND_ZERO t1 l1 l2) =
  do addVar t1
     return $ "\t" ++ "beq " ++ t1 ++ ", " ++ "$zero" ++ ", " ++ l2 ++ "\n"

-- Has to be the inverse operation
decodeCond (COND t1 op t2 l1 l2) =
  do
    let str = case op of
          R_EQ -> "bne "
          R_NEQ -> "beq "
          R_GREATER -> "ble "
          R_GREATEREQ -> "blt "
          R_LESS -> "bge "
          R_LESSEQ -> "bgt "
    addVar t1
    addVar t2
    return $ "\t" ++ str ++ "$" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l2 ++ "\n"

decodeFuncCall :: [Instr] -> State VarTrack String
decodeFuncCall [] = return ""
decodeFuncCall ((CALL id args):xs) =
  do (x, y, z) <- getInfo
     resetInfoTemp
     body <- decodeInstr xs
     (_, y', _) <- getInfo
     let list = intersect (Set.toList y) (Set.toList y')
     restoreInfo (x, y, z)
     start <- saveInStack list
     end <- deleteFromStack list
     argcode <- decodeCallArgs args 0
     return (start ++ argcode ++ "\t" ++ "jal " ++ id ++ "\n" ++ end ++ body)

decodeCallArgs :: [Temp] -> Int -> State VarTrack String
decodeCallArgs [] _ = return ""
decodeCallArgs (x:xs) n = 
  do
    let code1 = "\tmove $a" ++ show n ++ ", $" ++ x ++ "\n"
    code2 <- decodeCallArgs xs (n+1)
    return (code1 ++ code2)


decodeReturn :: Instr -> State VarTrack String
decodeReturn something =
  do
     let str = case something of
                 RET -> "\tjr $ra\n"
                 RETVAL t -> "\tmove $v0, $" ++ t ++ "\n\tjr $ra\n"
                 _ -> "Something wrong probably string related" ++ "\n"
     return str
