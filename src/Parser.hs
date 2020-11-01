{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,240) ([0,0,112,0,0,448,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,112,0,0,448,0,0,0,16,0,0,1,0,0,0,0,0,0,0,0,64,0,0,28,0,0,112,0,0,0,4,0,8192,0,0,32768,0,0,0,8,0,0,34,0,0,1024,0,0,28,0,49158,1161,1,0,0,0,96,18588,16,0,16384,0,0,0,8,57344,255,0,0,0,0,0,0,0,0,0,1024,0,0,4096,0,0,16384,0,384,0,67,1536,35264,260,6144,0,1040,0,0,64,0,0,0,0,6,1536,1,65504,2048,0,0,4096,0,65024,15,2,0,0,0,6144,0,1040,24576,0,4160,32768,1,17152,0,0,0,0,0,0,0,96,16384,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,32768,0,1536,0,260,57344,255,8,32768,1023,32,0,0,0,0,0,0,0,0,0,0,65408,3,4,0,32768,0,0,0,0,0,0,0,24576,0,4160,32768,28673,16674,0,49158,1161,1,65504,8192,0,96,18588,16,384,0,65,63488,63,8,0,0,0,24576,0,4160,0,0,0,0,0,0,0,0,0,0,96,18588,16,0,0,0,1536,0,260,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","GDecl","GDef","Stmt","MultStmt","Simple","Type","Exp","ArithmeticOp","RelationalOp","BinOp","FuncParams","CallParams","ForInner","int","bool","\"+\"","\"-\"","\"/\"","\"*\"","\"%\"","\"==\"","\"<=\"","\">=\"","\"!=\"","\"<\"","\">\"","\"for\"","\"while\"","\"if\"","\"then\"","\"else\"","\"return\"","\"int\"","\"char\"","\"bool\"","\"{\"","\"}\"","\")\"","\"(\"","\";\"","\"[\"","\"]\"","\",\"","\"=\"","id","%eof"]
        bit_start = st * 50
        bit_end = (st + 1) * 50
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..49]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (37) = happyShift action_4
action_0 (38) = happyShift action_5
action_0 (39) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (5) = happyGoto action_8
action_0 (6) = happyGoto action_9
action_0 (10) = happyGoto action_10
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (37) = happyShift action_4
action_1 (38) = happyShift action_5
action_1 (39) = happyShift action_6
action_1 (5) = happyGoto action_2
action_1 (10) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyFail (happyExpListPerState 2)

action_3 (49) = happyShift action_14
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_18

action_5 _ = happyReduce_20

action_6 _ = happyReduce_19

action_7 (50) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (37) = happyShift action_4
action_8 (38) = happyShift action_5
action_8 (39) = happyShift action_6
action_8 (4) = happyGoto action_13
action_8 (5) = happyGoto action_8
action_8 (6) = happyGoto action_9
action_8 (10) = happyGoto action_10
action_8 _ = happyReduce_1

action_9 (37) = happyShift action_4
action_9 (38) = happyShift action_5
action_9 (39) = happyShift action_6
action_9 (4) = happyGoto action_12
action_9 (5) = happyGoto action_8
action_9 (6) = happyGoto action_9
action_9 (10) = happyGoto action_10
action_9 _ = happyReduce_2

action_10 (49) = happyShift action_11
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (43) = happyShift action_16
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_4

action_13 _ = happyReduce_3

action_14 (43) = happyShift action_15
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (37) = happyShift action_4
action_15 (38) = happyShift action_5
action_15 (39) = happyShift action_6
action_15 (10) = happyGoto action_17
action_15 (15) = happyGoto action_19
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (37) = happyShift action_4
action_16 (38) = happyShift action_5
action_16 (39) = happyShift action_6
action_16 (10) = happyGoto action_17
action_16 (15) = happyGoto action_18
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (49) = happyShift action_22
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (42) = happyShift action_21
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (42) = happyShift action_20
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (44) = happyShift action_25
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (40) = happyShift action_24
action_21 (44) = happyShift action_25
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (47) = happyShift action_23
action_22 _ = happyReduce_41

action_23 (37) = happyShift action_4
action_23 (38) = happyShift action_5
action_23 (39) = happyShift action_6
action_23 (10) = happyGoto action_17
action_23 (15) = happyGoto action_39
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (18) = happyShift action_30
action_24 (19) = happyShift action_31
action_24 (31) = happyShift action_32
action_24 (32) = happyShift action_33
action_24 (33) = happyShift action_34
action_24 (36) = happyShift action_35
action_24 (40) = happyShift action_36
action_24 (43) = happyShift action_37
action_24 (49) = happyShift action_38
action_24 (7) = happyGoto action_26
action_24 (8) = happyGoto action_27
action_24 (9) = happyGoto action_28
action_24 (11) = happyGoto action_29
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_5

action_26 (18) = happyShift action_30
action_26 (19) = happyShift action_31
action_26 (31) = happyShift action_32
action_26 (32) = happyShift action_33
action_26 (33) = happyShift action_34
action_26 (36) = happyShift action_35
action_26 (40) = happyShift action_36
action_26 (43) = happyShift action_37
action_26 (49) = happyShift action_38
action_26 (7) = happyGoto action_26
action_26 (8) = happyGoto action_64
action_26 (9) = happyGoto action_28
action_26 (11) = happyGoto action_29
action_26 _ = happyReduce_15

action_27 (41) = happyShift action_63
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (44) = happyShift action_62
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (20) = happyShift action_51
action_29 (21) = happyShift action_52
action_29 (22) = happyShift action_53
action_29 (23) = happyShift action_54
action_29 (24) = happyShift action_55
action_29 (25) = happyShift action_56
action_29 (26) = happyShift action_57
action_29 (27) = happyShift action_58
action_29 (28) = happyShift action_59
action_29 (29) = happyShift action_60
action_29 (30) = happyShift action_61
action_29 (12) = happyGoto action_48
action_29 (13) = happyGoto action_49
action_29 (14) = happyGoto action_50
action_29 _ = happyReduce_17

action_30 _ = happyReduce_22

action_31 _ = happyReduce_23

action_32 (43) = happyShift action_47
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (43) = happyShift action_46
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (43) = happyShift action_45
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (18) = happyShift action_30
action_35 (19) = happyShift action_31
action_35 (43) = happyShift action_37
action_35 (44) = happyShift action_44
action_35 (49) = happyShift action_38
action_35 (11) = happyGoto action_43
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (18) = happyShift action_30
action_36 (19) = happyShift action_31
action_36 (31) = happyShift action_32
action_36 (32) = happyShift action_33
action_36 (33) = happyShift action_34
action_36 (36) = happyShift action_35
action_36 (40) = happyShift action_36
action_36 (43) = happyShift action_37
action_36 (49) = happyShift action_38
action_36 (7) = happyGoto action_26
action_36 (8) = happyGoto action_42
action_36 (9) = happyGoto action_28
action_36 (11) = happyGoto action_29
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (18) = happyShift action_30
action_37 (19) = happyShift action_31
action_37 (43) = happyShift action_37
action_37 (49) = happyShift action_38
action_37 (11) = happyGoto action_41
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (43) = happyShift action_40
action_38 _ = happyReduce_25

action_39 _ = happyReduce_42

action_40 (18) = happyShift action_30
action_40 (19) = happyShift action_31
action_40 (42) = happyShift action_76
action_40 (43) = happyShift action_37
action_40 (49) = happyShift action_38
action_40 (11) = happyGoto action_74
action_40 (16) = happyGoto action_75
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (20) = happyShift action_51
action_41 (21) = happyShift action_52
action_41 (22) = happyShift action_53
action_41 (23) = happyShift action_54
action_41 (24) = happyShift action_55
action_41 (25) = happyShift action_56
action_41 (26) = happyShift action_57
action_41 (27) = happyShift action_58
action_41 (28) = happyShift action_59
action_41 (29) = happyShift action_60
action_41 (30) = happyShift action_61
action_41 (42) = happyShift action_73
action_41 (12) = happyGoto action_48
action_41 (13) = happyGoto action_49
action_41 (14) = happyGoto action_50
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (41) = happyShift action_72
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (20) = happyShift action_51
action_43 (21) = happyShift action_52
action_43 (22) = happyShift action_53
action_43 (23) = happyShift action_54
action_43 (24) = happyShift action_55
action_43 (25) = happyShift action_56
action_43 (26) = happyShift action_57
action_43 (27) = happyShift action_58
action_43 (28) = happyShift action_59
action_43 (29) = happyShift action_60
action_43 (30) = happyShift action_61
action_43 (44) = happyShift action_71
action_43 (12) = happyGoto action_48
action_43 (13) = happyGoto action_49
action_43 (14) = happyGoto action_50
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_12

action_45 (18) = happyShift action_30
action_45 (19) = happyShift action_31
action_45 (43) = happyShift action_37
action_45 (49) = happyShift action_38
action_45 (11) = happyGoto action_70
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (18) = happyShift action_30
action_46 (19) = happyShift action_31
action_46 (43) = happyShift action_37
action_46 (49) = happyShift action_38
action_46 (11) = happyGoto action_69
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (18) = happyShift action_30
action_47 (19) = happyShift action_31
action_47 (43) = happyShift action_37
action_47 (44) = happyShift action_68
action_47 (49) = happyShift action_38
action_47 (9) = happyGoto action_66
action_47 (11) = happyGoto action_29
action_47 (17) = happyGoto action_67
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_39

action_49 _ = happyReduce_40

action_50 (18) = happyShift action_30
action_50 (19) = happyShift action_31
action_50 (43) = happyShift action_37
action_50 (49) = happyShift action_38
action_50 (11) = happyGoto action_65
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_28

action_52 _ = happyReduce_29

action_53 _ = happyReduce_30

action_54 _ = happyReduce_31

action_55 _ = happyReduce_32

action_56 _ = happyReduce_33

action_57 _ = happyReduce_34

action_58 _ = happyReduce_35

action_59 _ = happyReduce_36

action_60 _ = happyReduce_37

action_61 _ = happyReduce_38

action_62 _ = happyReduce_7

action_63 _ = happyReduce_6

action_64 _ = happyReduce_16

action_65 (20) = happyShift action_51
action_65 (21) = happyShift action_52
action_65 (22) = happyShift action_53
action_65 (23) = happyShift action_54
action_65 (24) = happyShift action_55
action_65 (25) = happyShift action_56
action_65 (26) = happyShift action_57
action_65 (27) = happyShift action_58
action_65 (28) = happyShift action_59
action_65 (29) = happyShift action_60
action_65 (30) = happyShift action_61
action_65 (12) = happyGoto action_48
action_65 (13) = happyGoto action_49
action_65 (14) = happyGoto action_50
action_65 _ = happyReduce_24

action_66 (44) = happyShift action_83
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (42) = happyShift action_82
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (18) = happyShift action_30
action_68 (19) = happyShift action_31
action_68 (43) = happyShift action_37
action_68 (49) = happyShift action_38
action_68 (11) = happyGoto action_81
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (20) = happyShift action_51
action_69 (21) = happyShift action_52
action_69 (22) = happyShift action_53
action_69 (23) = happyShift action_54
action_69 (24) = happyShift action_55
action_69 (25) = happyShift action_56
action_69 (26) = happyShift action_57
action_69 (27) = happyShift action_58
action_69 (28) = happyShift action_59
action_69 (29) = happyShift action_60
action_69 (30) = happyShift action_61
action_69 (42) = happyShift action_80
action_69 (12) = happyGoto action_48
action_69 (13) = happyGoto action_49
action_69 (14) = happyGoto action_50
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (20) = happyShift action_51
action_70 (21) = happyShift action_52
action_70 (22) = happyShift action_53
action_70 (23) = happyShift action_54
action_70 (24) = happyShift action_55
action_70 (25) = happyShift action_56
action_70 (26) = happyShift action_57
action_70 (27) = happyShift action_58
action_70 (28) = happyShift action_59
action_70 (29) = happyShift action_60
action_70 (30) = happyShift action_61
action_70 (42) = happyShift action_79
action_70 (12) = happyGoto action_48
action_70 (13) = happyGoto action_49
action_70 (14) = happyGoto action_50
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_13

action_72 _ = happyReduce_14

action_73 _ = happyReduce_21

action_74 (20) = happyShift action_51
action_74 (21) = happyShift action_52
action_74 (22) = happyShift action_53
action_74 (23) = happyShift action_54
action_74 (24) = happyShift action_55
action_74 (25) = happyShift action_56
action_74 (26) = happyShift action_57
action_74 (27) = happyShift action_58
action_74 (28) = happyShift action_59
action_74 (29) = happyShift action_60
action_74 (30) = happyShift action_61
action_74 (47) = happyShift action_78
action_74 (12) = happyGoto action_48
action_74 (13) = happyGoto action_49
action_74 (14) = happyGoto action_50
action_74 _ = happyReduce_43

action_75 (42) = happyShift action_77
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_26

action_77 _ = happyReduce_27

action_78 (18) = happyShift action_30
action_78 (19) = happyShift action_31
action_78 (43) = happyShift action_37
action_78 (49) = happyShift action_38
action_78 (11) = happyGoto action_74
action_78 (16) = happyGoto action_89
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (18) = happyShift action_30
action_79 (19) = happyShift action_31
action_79 (31) = happyShift action_32
action_79 (32) = happyShift action_33
action_79 (33) = happyShift action_34
action_79 (36) = happyShift action_35
action_79 (40) = happyShift action_36
action_79 (43) = happyShift action_37
action_79 (49) = happyShift action_38
action_79 (7) = happyGoto action_88
action_79 (9) = happyGoto action_28
action_79 (11) = happyGoto action_29
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (18) = happyShift action_30
action_80 (19) = happyShift action_31
action_80 (31) = happyShift action_32
action_80 (32) = happyShift action_33
action_80 (33) = happyShift action_34
action_80 (36) = happyShift action_35
action_80 (40) = happyShift action_36
action_80 (43) = happyShift action_37
action_80 (49) = happyShift action_38
action_80 (7) = happyGoto action_87
action_80 (9) = happyGoto action_28
action_80 (11) = happyGoto action_29
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (20) = happyShift action_51
action_81 (21) = happyShift action_52
action_81 (22) = happyShift action_53
action_81 (23) = happyShift action_54
action_81 (24) = happyShift action_55
action_81 (25) = happyShift action_56
action_81 (26) = happyShift action_57
action_81 (27) = happyShift action_58
action_81 (28) = happyShift action_59
action_81 (29) = happyShift action_60
action_81 (30) = happyShift action_61
action_81 (44) = happyShift action_86
action_81 (12) = happyGoto action_48
action_81 (13) = happyGoto action_49
action_81 (14) = happyGoto action_50
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (18) = happyShift action_30
action_82 (19) = happyShift action_31
action_82 (31) = happyShift action_32
action_82 (32) = happyShift action_33
action_82 (33) = happyShift action_34
action_82 (36) = happyShift action_35
action_82 (40) = happyShift action_36
action_82 (43) = happyShift action_37
action_82 (49) = happyShift action_38
action_82 (7) = happyGoto action_85
action_82 (9) = happyGoto action_28
action_82 (11) = happyGoto action_29
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (18) = happyShift action_30
action_83 (19) = happyShift action_31
action_83 (43) = happyShift action_37
action_83 (49) = happyShift action_38
action_83 (11) = happyGoto action_84
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (20) = happyShift action_51
action_84 (21) = happyShift action_52
action_84 (22) = happyShift action_53
action_84 (23) = happyShift action_54
action_84 (24) = happyShift action_55
action_84 (25) = happyShift action_56
action_84 (26) = happyShift action_57
action_84 (27) = happyShift action_58
action_84 (28) = happyShift action_59
action_84 (29) = happyShift action_60
action_84 (30) = happyShift action_61
action_84 (44) = happyShift action_92
action_84 (12) = happyGoto action_48
action_84 (13) = happyGoto action_49
action_84 (14) = happyGoto action_50
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_11

action_86 (18) = happyShift action_30
action_86 (19) = happyShift action_31
action_86 (43) = happyShift action_37
action_86 (49) = happyShift action_38
action_86 (9) = happyGoto action_91
action_86 (11) = happyGoto action_29
action_86 _ = happyReduce_45

action_87 _ = happyReduce_10

action_88 (35) = happyShift action_90
action_88 _ = happyReduce_8

action_89 _ = happyReduce_44

action_90 (18) = happyShift action_30
action_90 (19) = happyShift action_31
action_90 (31) = happyShift action_32
action_90 (32) = happyShift action_33
action_90 (33) = happyShift action_34
action_90 (36) = happyShift action_35
action_90 (40) = happyShift action_36
action_90 (43) = happyShift action_37
action_90 (49) = happyShift action_38
action_90 (7) = happyGoto action_94
action_90 (9) = happyGoto action_28
action_90 (11) = happyGoto action_29
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_47

action_92 (18) = happyShift action_30
action_92 (19) = happyShift action_31
action_92 (43) = happyShift action_37
action_92 (49) = happyShift action_38
action_92 (9) = happyGoto action_93
action_92 (11) = happyGoto action_29
action_92 _ = happyReduce_46

action_93 _ = happyReduce_48

action_94 _ = happyReduce_9

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 _
	_
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_4 = happySpecReduce_2  4 happyReduction_4
happyReduction_4 _
	_
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_5 = happyReduce 6 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (()
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 8 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (()
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 _
	_
	 =  HappyAbsSyn7
		 (()
	)

happyReduce_8 = happyReduce 5 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (()
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 7 7 happyReduction_9
happyReduction_9 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (()
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 5 7 happyReduction_10
happyReduction_10 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (()
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 7 happyReduction_11
happyReduction_11 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (()
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_2  7 happyReduction_12
happyReduction_12 _
	_
	 =  HappyAbsSyn7
		 (()
	)

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 _
	_
	_
	 =  HappyAbsSyn7
		 (()
	)

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 _
	_
	_
	 =  HappyAbsSyn7
		 (()
	)

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn8
		 (()
	)

happyReduce_16 = happySpecReduce_2  8 happyReduction_16
happyReduction_16 _
	_
	 =  HappyAbsSyn8
		 (()
	)

happyReduce_17 = happySpecReduce_1  9 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn9
		 (()
	)

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn10
		 (()
	)

happyReduce_19 = happySpecReduce_1  10 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn10
		 (()
	)

happyReduce_20 = happySpecReduce_1  10 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn10
		 (()
	)

happyReduce_21 = happySpecReduce_3  11 happyReduction_21
happyReduction_21 _
	_
	_
	 =  HappyAbsSyn11
		 (()
	)

happyReduce_22 = happySpecReduce_1  11 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn11
		 (()
	)

happyReduce_23 = happySpecReduce_1  11 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn11
		 (()
	)

happyReduce_24 = happySpecReduce_3  11 happyReduction_24
happyReduction_24 _
	_
	_
	 =  HappyAbsSyn11
		 (()
	)

happyReduce_25 = happySpecReduce_1  11 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn11
		 (()
	)

happyReduce_26 = happySpecReduce_3  11 happyReduction_26
happyReduction_26 _
	_
	_
	 =  HappyAbsSyn11
		 (()
	)

happyReduce_27 = happyReduce 4 11 happyReduction_27
happyReduction_27 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (()
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  12 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn12
		 (()
	)

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn12
		 (()
	)

happyReduce_30 = happySpecReduce_1  12 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn12
		 (()
	)

happyReduce_31 = happySpecReduce_1  12 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn12
		 (()
	)

happyReduce_32 = happySpecReduce_1  12 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn12
		 (()
	)

happyReduce_33 = happySpecReduce_1  13 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn13
		 (()
	)

happyReduce_34 = happySpecReduce_1  13 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn13
		 (()
	)

happyReduce_35 = happySpecReduce_1  13 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn13
		 (()
	)

happyReduce_36 = happySpecReduce_1  13 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn13
		 (()
	)

happyReduce_37 = happySpecReduce_1  13 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn13
		 (()
	)

happyReduce_38 = happySpecReduce_1  13 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn13
		 (()
	)

happyReduce_39 = happySpecReduce_1  14 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn14
		 (()
	)

happyReduce_40 = happySpecReduce_1  14 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn14
		 (()
	)

happyReduce_41 = happySpecReduce_2  15 happyReduction_41
happyReduction_41 _
	_
	 =  HappyAbsSyn15
		 (()
	)

happyReduce_42 = happyReduce 4 15 happyReduction_42
happyReduction_42 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (()
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_1  16 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn16
		 (()
	)

happyReduce_44 = happySpecReduce_3  16 happyReduction_44
happyReduction_44 _
	_
	_
	 =  HappyAbsSyn16
		 (()
	)

happyReduce_45 = happySpecReduce_3  17 happyReduction_45
happyReduction_45 _
	_
	_
	 =  HappyAbsSyn17
		 (()
	)

happyReduce_46 = happyReduce 4 17 happyReduction_46
happyReduction_46 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (()
	) `HappyStk` happyRest

happyReduce_47 = happyReduce 4 17 happyReduction_47
happyReduction_47 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (()
	) `HappyStk` happyRest

happyReduce_48 = happyReduce 5 17 happyReduction_48
happyReduction_48 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (()
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 50 50 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	INT happy_dollar_dollar -> cont 18;
	BOOL happy_dollar_dollar -> cont 19;
	A_OP ADD -> cont 20;
	A_OP SUB -> cont 21;
	A_OP DIV -> cont 22;
	A_OP MULT -> cont 23;
	A_OP MOD -> cont 24;
	R_OP EQUAL -> cont 25;
	R_OP LESS_OR_EQUAL -> cont 26;
	R_OP MORE_OR_EQUAL -> cont 27;
	R_OP NOT_EQUAL -> cont 28;
	R_OP LESS -> cont 29;
	R_OP MORE -> cont 30;
	FOR -> cont 31;
	WHILE -> cont 32;
	IF -> cont 33;
	THEN -> cont 34;
	ELSE -> cont 35;
	RETURN -> cont 36;
	T_INT -> cont 37;
	T_CHAR -> cont 38;
	T_BOOL -> cont 39;
	LBRACE -> cont 40;
	RBRACE -> cont 41;
	RPAREN -> cont 42;
	LPAREN -> cont 43;
	SEMICOLON -> cont 44;
	LBRACKET -> cont 45;
	RBRACKET -> cont 46;
	COMMA -> cont 47;
	ASSIGN -> cont 48;
	ID -> cont 49;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 50 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
    parseError toks = print "OLA"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
