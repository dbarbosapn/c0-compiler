module MachineSym where

import MiddleCode

{- To do: Add immediate, mips dosent have subi and muli so we need to do that in
          typecheck
          Move S, need to allocate an array to do this-}

generateMips :: [Function] -> IO()
generateMips something =
  case something of
    [] -> putStrLn ""
    list -> do decodeFunction $ head list
               generateMips $ tail list

-- Needs more work
decodeFunction :: Function -> IO()
decodeFunction (Def id args inst) =
  do putStrLn $ "_" ++ id ++ "_:"
     decodeInstr inst

-- Needs more work
decodeReturn :: Instr -> IO()
decodeReturn something =
  do case something of
       RET -> putStrLn $ "\tReturn"
       RETVAL t -> putStrLn $ "\tReturn " ++ t
       _ -> putStrLn $ "Something wrong"

-- Need more work
decodeFuncCall :: Instr -> IO()
decodeFuncCall (CALL id args) =
  do putStrLn $ "\t" ++ "call " ++ id

decodeInstr :: [Instr] -> IO()
decodeInstr [] = putStr ""
decodeInstr (x:xs) =
  do case x of
       OP _ _ _ _ -> decodeInstArithmetic x
       MOVE t1 t2 -> putStrLn $ "\t" ++ "move " ++ t1 ++ ", " ++ t2
       MOVEI t1 n -> putStrLn $ "\t" ++ "li " ++ t1 ++ ", " ++ (show n)
       CALL _ _ -> decodeFuncCall x
       LABEL l1 -> putStrLn $ l1 ++ ":"
       COND _ _ _ _ _ -> decodeWhile x
       COND_ZERO _ _ _ -> decodeWhile x
       JUMP l -> putStrLn $ "\t" ++ "jump " ++ l
       _ -> decodeReturn x
     decodeInstr xs

decodeInstArithmetic :: Instr -> IO()
decodeInstArithmetic (OP op t1 t2 t3) =
  case op of
    A_ADD -> putStrLn $ "\t" ++ "add " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3
    A_MULT -> putStrLn $ "\t" ++ "mul " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3
    A_SUB -> putStrLn $ "\t" ++ "sub " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3
    _ -> do putStrLn $ "\t" ++ "div" ++ t2 ++ ", " ++ t3
            case op of
              A_DIV -> putStrLn $ "\t" ++ "mflo " ++ t1
              A_MOD -> putStrLn $ "\t" ++ "mfhi " ++ t1

-- Our middle code does alot, so the while statement its simple to do here
decodeWhile :: Instr -> IO()
decodeWhile (COND_ZERO t1 l1 l2) =
  putStrLn $ "\t" ++ "beq " ++ t1 ++ ", " ++ "$zero" ++ ", " ++ l2

decodeWhile (COND t1 op t2 l1 l2) =
  do let str = case op of
                 R_EQ -> "beq "
                 R_NEQ -> "bne "
                 R_GREATER -> "bgt "
                 R_GREATEREQ -> "bge "
                 R_LESS -> "blt "
                 R_LESSEQ -> "ble "
     putStrLn $ "\t" ++ str ++ t1 ++ ", " ++ t2 ++ ", " ++ l2
