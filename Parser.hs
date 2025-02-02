{-# OPTIONS_GHC -w #-}
module Parser where 

import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,342) ([4128,42087,512,0,0,0,0,32762,2630,4128,42087,0,0,0,0,0,28930,2630,0,0,0,512,0,8192,0,28930,2630,4128,42087,64000,26239,40970,26623,228,0,128,0,128,64000,18175,10,240,0,0,0,4128,42087,512,18033,8202,26384,164,28930,2630,4128,42087,512,18033,8202,26384,164,28930,2630,0,0,0,0,0,0,0,3840,0,61440,0,49152,15,0,254,0,4064,0,4128,42087,0,0,8290,26384,164,0,0,4128,42087,64000,18047,40986,26623,180,4,0,0,8192,6,0,0,0,0,32762,2631,4128,42087,1024,0,8192,26384,164,28930,2630,0,256,512,0,0,0,0,0,0,0,8192,63494,15,0,0,64,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Exp","Type","num","\"->\"","'+'","'-'","'*'","\"&&\"","\"||\"","'!'","\"==\"","\">=\"","'>'","true","false","if","then","else","var","'\\\\'","':'","'.'","','","let","'='","in","'('","')'","'{'","'}'","Bool","Number","%eof"]
        bit_start = st Prelude.* 36
        bit_end = (st Prelude.+ 1) Prelude.* 36
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..35]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (6) = happyShift action_2
action_0 (13) = happyShift action_4
action_0 (17) = happyShift action_5
action_0 (18) = happyShift action_6
action_0 (19) = happyShift action_7
action_0 (22) = happyShift action_8
action_0 (23) = happyShift action_9
action_0 (27) = happyShift action_10
action_0 (30) = happyShift action_11
action_0 (32) = happyShift action_12
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (6) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (6) = happyShift action_2
action_3 (8) = happyShift action_20
action_3 (9) = happyShift action_21
action_3 (10) = happyShift action_22
action_3 (11) = happyShift action_23
action_3 (12) = happyShift action_24
action_3 (13) = happyShift action_4
action_3 (14) = happyShift action_25
action_3 (15) = happyShift action_26
action_3 (16) = happyShift action_27
action_3 (17) = happyShift action_5
action_3 (18) = happyShift action_6
action_3 (19) = happyShift action_7
action_3 (22) = happyShift action_8
action_3 (23) = happyShift action_9
action_3 (27) = happyShift action_10
action_3 (30) = happyShift action_11
action_3 (32) = happyShift action_12
action_3 (36) = happyAccept
action_3 (4) = happyGoto action_19
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (6) = happyShift action_2
action_4 (13) = happyShift action_4
action_4 (17) = happyShift action_5
action_4 (18) = happyShift action_6
action_4 (19) = happyShift action_7
action_4 (22) = happyShift action_8
action_4 (23) = happyShift action_9
action_4 (27) = happyShift action_10
action_4 (30) = happyShift action_11
action_4 (32) = happyShift action_12
action_4 (4) = happyGoto action_18
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_4

action_6 _ = happyReduce_3

action_7 (6) = happyShift action_2
action_7 (13) = happyShift action_4
action_7 (17) = happyShift action_5
action_7 (18) = happyShift action_6
action_7 (19) = happyShift action_7
action_7 (22) = happyShift action_8
action_7 (23) = happyShift action_9
action_7 (27) = happyShift action_10
action_7 (30) = happyShift action_11
action_7 (32) = happyShift action_12
action_7 (4) = happyGoto action_17
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_2

action_9 (22) = happyShift action_16
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (22) = happyShift action_15
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (6) = happyShift action_2
action_11 (13) = happyShift action_4
action_11 (17) = happyShift action_5
action_11 (18) = happyShift action_6
action_11 (19) = happyShift action_7
action_11 (22) = happyShift action_8
action_11 (23) = happyShift action_9
action_11 (27) = happyShift action_10
action_11 (30) = happyShift action_11
action_11 (32) = happyShift action_12
action_11 (4) = happyGoto action_14
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (6) = happyShift action_2
action_12 (13) = happyShift action_4
action_12 (17) = happyShift action_5
action_12 (18) = happyShift action_6
action_12 (19) = happyShift action_7
action_12 (22) = happyShift action_8
action_12 (23) = happyShift action_9
action_12 (27) = happyShift action_10
action_12 (30) = happyShift action_11
action_12 (32) = happyShift action_12
action_12 (4) = happyGoto action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (6) = happyShift action_2
action_13 (8) = happyShift action_20
action_13 (9) = happyShift action_21
action_13 (10) = happyShift action_22
action_13 (11) = happyShift action_23
action_13 (12) = happyShift action_24
action_13 (13) = happyShift action_4
action_13 (14) = happyShift action_25
action_13 (15) = happyShift action_26
action_13 (16) = happyShift action_27
action_13 (17) = happyShift action_5
action_13 (18) = happyShift action_6
action_13 (19) = happyShift action_7
action_13 (22) = happyShift action_8
action_13 (23) = happyShift action_9
action_13 (26) = happyShift action_40
action_13 (27) = happyShift action_10
action_13 (30) = happyShift action_11
action_13 (32) = happyShift action_12
action_13 (4) = happyGoto action_19
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (6) = happyShift action_2
action_14 (8) = happyShift action_20
action_14 (9) = happyShift action_21
action_14 (10) = happyShift action_22
action_14 (11) = happyShift action_23
action_14 (12) = happyShift action_24
action_14 (13) = happyShift action_4
action_14 (14) = happyShift action_25
action_14 (15) = happyShift action_26
action_14 (16) = happyShift action_27
action_14 (17) = happyShift action_5
action_14 (18) = happyShift action_6
action_14 (19) = happyShift action_7
action_14 (22) = happyShift action_8
action_14 (23) = happyShift action_9
action_14 (27) = happyShift action_10
action_14 (30) = happyShift action_11
action_14 (31) = happyShift action_39
action_14 (32) = happyShift action_12
action_14 (4) = happyGoto action_19
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (28) = happyShift action_38
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (24) = happyShift action_37
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (6) = happyShift action_2
action_17 (8) = happyShift action_20
action_17 (9) = happyShift action_21
action_17 (10) = happyShift action_22
action_17 (11) = happyShift action_23
action_17 (12) = happyShift action_24
action_17 (13) = happyShift action_4
action_17 (14) = happyShift action_25
action_17 (15) = happyShift action_26
action_17 (16) = happyShift action_27
action_17 (17) = happyShift action_5
action_17 (18) = happyShift action_6
action_17 (19) = happyShift action_7
action_17 (20) = happyShift action_36
action_17 (22) = happyShift action_8
action_17 (23) = happyShift action_9
action_17 (27) = happyShift action_10
action_17 (30) = happyShift action_11
action_17 (32) = happyShift action_12
action_17 (4) = happyGoto action_19
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (6) = happyShift action_2
action_18 (13) = happyShift action_4
action_18 (14) = happyShift action_25
action_18 (15) = happyShift action_26
action_18 (16) = happyShift action_27
action_18 (17) = happyShift action_5
action_18 (18) = happyShift action_6
action_18 (22) = happyShift action_8
action_18 (23) = happyShift action_9
action_18 (27) = happyShift action_10
action_18 (30) = happyShift action_11
action_18 (32) = happyShift action_12
action_18 (4) = happyGoto action_19
action_18 _ = happyReduce_10

action_19 (6) = happyShift action_2
action_19 (8) = happyShift action_20
action_19 (9) = happyShift action_21
action_19 (10) = happyShift action_22
action_19 (11) = happyShift action_23
action_19 (12) = happyShift action_24
action_19 (13) = happyShift action_4
action_19 (14) = happyShift action_25
action_19 (15) = happyShift action_26
action_19 (16) = happyShift action_27
action_19 (17) = happyShift action_5
action_19 (18) = happyShift action_6
action_19 (19) = happyShift action_7
action_19 (22) = happyShift action_8
action_19 (23) = happyShift action_9
action_19 (27) = happyShift action_10
action_19 (30) = happyShift action_11
action_19 (32) = happyShift action_12
action_19 (4) = happyGoto action_19
action_19 _ = happyReduce_14

action_20 (6) = happyShift action_2
action_20 (13) = happyShift action_4
action_20 (17) = happyShift action_5
action_20 (18) = happyShift action_6
action_20 (19) = happyShift action_7
action_20 (22) = happyShift action_8
action_20 (23) = happyShift action_9
action_20 (27) = happyShift action_10
action_20 (30) = happyShift action_11
action_20 (32) = happyShift action_12
action_20 (4) = happyGoto action_35
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (6) = happyShift action_2
action_21 (13) = happyShift action_4
action_21 (17) = happyShift action_5
action_21 (18) = happyShift action_6
action_21 (19) = happyShift action_7
action_21 (22) = happyShift action_8
action_21 (23) = happyShift action_9
action_21 (27) = happyShift action_10
action_21 (30) = happyShift action_11
action_21 (32) = happyShift action_12
action_21 (4) = happyGoto action_34
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (6) = happyShift action_2
action_22 (13) = happyShift action_4
action_22 (17) = happyShift action_5
action_22 (18) = happyShift action_6
action_22 (19) = happyShift action_7
action_22 (22) = happyShift action_8
action_22 (23) = happyShift action_9
action_22 (27) = happyShift action_10
action_22 (30) = happyShift action_11
action_22 (32) = happyShift action_12
action_22 (4) = happyGoto action_33
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (6) = happyShift action_2
action_23 (13) = happyShift action_4
action_23 (17) = happyShift action_5
action_23 (18) = happyShift action_6
action_23 (19) = happyShift action_7
action_23 (22) = happyShift action_8
action_23 (23) = happyShift action_9
action_23 (27) = happyShift action_10
action_23 (30) = happyShift action_11
action_23 (32) = happyShift action_12
action_23 (4) = happyGoto action_32
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (6) = happyShift action_2
action_24 (13) = happyShift action_4
action_24 (17) = happyShift action_5
action_24 (18) = happyShift action_6
action_24 (19) = happyShift action_7
action_24 (22) = happyShift action_8
action_24 (23) = happyShift action_9
action_24 (27) = happyShift action_10
action_24 (30) = happyShift action_11
action_24 (32) = happyShift action_12
action_24 (4) = happyGoto action_31
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (6) = happyShift action_2
action_25 (13) = happyShift action_4
action_25 (17) = happyShift action_5
action_25 (18) = happyShift action_6
action_25 (19) = happyShift action_7
action_25 (22) = happyShift action_8
action_25 (23) = happyShift action_9
action_25 (27) = happyShift action_10
action_25 (30) = happyShift action_11
action_25 (32) = happyShift action_12
action_25 (4) = happyGoto action_30
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (6) = happyShift action_2
action_26 (13) = happyShift action_4
action_26 (17) = happyShift action_5
action_26 (18) = happyShift action_6
action_26 (19) = happyShift action_7
action_26 (22) = happyShift action_8
action_26 (23) = happyShift action_9
action_26 (27) = happyShift action_10
action_26 (30) = happyShift action_11
action_26 (32) = happyShift action_12
action_26 (4) = happyGoto action_29
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (6) = happyShift action_2
action_27 (13) = happyShift action_4
action_27 (17) = happyShift action_5
action_27 (18) = happyShift action_6
action_27 (19) = happyShift action_7
action_27 (22) = happyShift action_8
action_27 (23) = happyShift action_9
action_27 (27) = happyShift action_10
action_27 (30) = happyShift action_11
action_27 (32) = happyShift action_12
action_27 (4) = happyGoto action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (6) = happyShift action_2
action_28 (17) = happyShift action_5
action_28 (18) = happyShift action_6
action_28 (22) = happyShift action_8
action_28 (23) = happyShift action_9
action_28 (27) = happyShift action_10
action_28 (30) = happyShift action_11
action_28 (32) = happyShift action_12
action_28 (4) = happyGoto action_19
action_28 _ = happyReduce_20

action_29 (6) = happyShift action_2
action_29 (17) = happyShift action_5
action_29 (18) = happyShift action_6
action_29 (22) = happyShift action_8
action_29 (23) = happyShift action_9
action_29 (27) = happyShift action_10
action_29 (30) = happyShift action_11
action_29 (32) = happyShift action_12
action_29 (4) = happyGoto action_19
action_29 _ = happyReduce_19

action_30 (6) = happyShift action_2
action_30 (17) = happyShift action_5
action_30 (18) = happyShift action_6
action_30 (22) = happyShift action_8
action_30 (23) = happyShift action_9
action_30 (27) = happyShift action_10
action_30 (30) = happyShift action_11
action_30 (32) = happyShift action_12
action_30 (4) = happyGoto action_19
action_30 _ = happyReduce_18

action_31 (6) = happyShift action_2
action_31 (13) = happyShift action_4
action_31 (14) = happyShift action_25
action_31 (15) = happyShift action_26
action_31 (16) = happyShift action_27
action_31 (17) = happyShift action_5
action_31 (18) = happyShift action_6
action_31 (22) = happyShift action_8
action_31 (23) = happyShift action_9
action_31 (27) = happyShift action_10
action_31 (30) = happyShift action_11
action_31 (32) = happyShift action_12
action_31 (4) = happyGoto action_19
action_31 _ = happyReduce_9

action_32 (6) = happyShift action_2
action_32 (13) = happyShift action_4
action_32 (14) = happyShift action_25
action_32 (15) = happyShift action_26
action_32 (16) = happyShift action_27
action_32 (17) = happyShift action_5
action_32 (18) = happyShift action_6
action_32 (22) = happyShift action_8
action_32 (23) = happyShift action_9
action_32 (27) = happyShift action_10
action_32 (30) = happyShift action_11
action_32 (32) = happyShift action_12
action_32 (4) = happyGoto action_19
action_32 _ = happyReduce_8

action_33 (6) = happyShift action_2
action_33 (11) = happyShift action_23
action_33 (12) = happyShift action_24
action_33 (13) = happyShift action_4
action_33 (14) = happyShift action_25
action_33 (15) = happyShift action_26
action_33 (16) = happyShift action_27
action_33 (17) = happyShift action_5
action_33 (18) = happyShift action_6
action_33 (22) = happyShift action_8
action_33 (23) = happyShift action_9
action_33 (27) = happyShift action_10
action_33 (30) = happyShift action_11
action_33 (32) = happyShift action_12
action_33 (4) = happyGoto action_19
action_33 _ = happyReduce_7

action_34 (6) = happyShift action_2
action_34 (10) = happyShift action_22
action_34 (11) = happyShift action_23
action_34 (12) = happyShift action_24
action_34 (13) = happyShift action_4
action_34 (14) = happyShift action_25
action_34 (15) = happyShift action_26
action_34 (16) = happyShift action_27
action_34 (17) = happyShift action_5
action_34 (18) = happyShift action_6
action_34 (22) = happyShift action_8
action_34 (23) = happyShift action_9
action_34 (27) = happyShift action_10
action_34 (30) = happyShift action_11
action_34 (32) = happyShift action_12
action_34 (4) = happyGoto action_19
action_34 _ = happyReduce_6

action_35 (6) = happyShift action_2
action_35 (10) = happyShift action_22
action_35 (11) = happyShift action_23
action_35 (12) = happyShift action_24
action_35 (13) = happyShift action_4
action_35 (14) = happyShift action_25
action_35 (15) = happyShift action_26
action_35 (16) = happyShift action_27
action_35 (17) = happyShift action_5
action_35 (18) = happyShift action_6
action_35 (22) = happyShift action_8
action_35 (23) = happyShift action_9
action_35 (27) = happyShift action_10
action_35 (30) = happyShift action_11
action_35 (32) = happyShift action_12
action_35 (4) = happyGoto action_19
action_35 _ = happyReduce_5

action_36 (6) = happyShift action_2
action_36 (13) = happyShift action_4
action_36 (17) = happyShift action_5
action_36 (18) = happyShift action_6
action_36 (19) = happyShift action_7
action_36 (22) = happyShift action_8
action_36 (23) = happyShift action_9
action_36 (27) = happyShift action_10
action_36 (30) = happyShift action_11
action_36 (32) = happyShift action_12
action_36 (4) = happyGoto action_47
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (30) = happyShift action_44
action_37 (34) = happyShift action_45
action_37 (35) = happyShift action_46
action_37 (5) = happyGoto action_43
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (6) = happyShift action_2
action_38 (13) = happyShift action_4
action_38 (17) = happyShift action_5
action_38 (18) = happyShift action_6
action_38 (19) = happyShift action_7
action_38 (22) = happyShift action_8
action_38 (23) = happyShift action_9
action_38 (27) = happyShift action_10
action_38 (30) = happyShift action_11
action_38 (32) = happyShift action_12
action_38 (4) = happyGoto action_42
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_15

action_40 (6) = happyShift action_2
action_40 (13) = happyShift action_4
action_40 (17) = happyShift action_5
action_40 (18) = happyShift action_6
action_40 (19) = happyShift action_7
action_40 (22) = happyShift action_8
action_40 (23) = happyShift action_9
action_40 (27) = happyShift action_10
action_40 (30) = happyShift action_11
action_40 (32) = happyShift action_12
action_40 (4) = happyGoto action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (6) = happyShift action_2
action_41 (8) = happyShift action_20
action_41 (9) = happyShift action_21
action_41 (10) = happyShift action_22
action_41 (11) = happyShift action_23
action_41 (12) = happyShift action_24
action_41 (13) = happyShift action_4
action_41 (14) = happyShift action_25
action_41 (15) = happyShift action_26
action_41 (16) = happyShift action_27
action_41 (17) = happyShift action_5
action_41 (18) = happyShift action_6
action_41 (19) = happyShift action_7
action_41 (22) = happyShift action_8
action_41 (23) = happyShift action_9
action_41 (27) = happyShift action_10
action_41 (30) = happyShift action_11
action_41 (32) = happyShift action_12
action_41 (33) = happyShift action_52
action_41 (4) = happyGoto action_19
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (6) = happyShift action_2
action_42 (8) = happyShift action_20
action_42 (9) = happyShift action_21
action_42 (10) = happyShift action_22
action_42 (11) = happyShift action_23
action_42 (12) = happyShift action_24
action_42 (13) = happyShift action_4
action_42 (14) = happyShift action_25
action_42 (15) = happyShift action_26
action_42 (16) = happyShift action_27
action_42 (17) = happyShift action_5
action_42 (18) = happyShift action_6
action_42 (19) = happyShift action_7
action_42 (22) = happyShift action_8
action_42 (23) = happyShift action_9
action_42 (27) = happyShift action_10
action_42 (29) = happyShift action_51
action_42 (30) = happyShift action_11
action_42 (32) = happyShift action_12
action_42 (4) = happyGoto action_19
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (7) = happyShift action_50
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (30) = happyShift action_44
action_44 (34) = happyShift action_45
action_44 (35) = happyShift action_46
action_44 (5) = happyGoto action_49
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_21

action_46 _ = happyReduce_22

action_47 (6) = happyShift action_2
action_47 (8) = happyShift action_20
action_47 (9) = happyShift action_21
action_47 (10) = happyShift action_22
action_47 (11) = happyShift action_23
action_47 (12) = happyShift action_24
action_47 (13) = happyShift action_4
action_47 (14) = happyShift action_25
action_47 (15) = happyShift action_26
action_47 (16) = happyShift action_27
action_47 (17) = happyShift action_5
action_47 (18) = happyShift action_6
action_47 (19) = happyShift action_7
action_47 (21) = happyShift action_48
action_47 (22) = happyShift action_8
action_47 (23) = happyShift action_9
action_47 (27) = happyShift action_10
action_47 (30) = happyShift action_11
action_47 (32) = happyShift action_12
action_47 (4) = happyGoto action_19
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (6) = happyShift action_2
action_48 (13) = happyShift action_4
action_48 (17) = happyShift action_5
action_48 (18) = happyShift action_6
action_48 (19) = happyShift action_7
action_48 (22) = happyShift action_8
action_48 (23) = happyShift action_9
action_48 (27) = happyShift action_10
action_48 (30) = happyShift action_11
action_48 (32) = happyShift action_12
action_48 (4) = happyGoto action_57
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (7) = happyShift action_56
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (6) = happyShift action_2
action_50 (13) = happyShift action_4
action_50 (17) = happyShift action_5
action_50 (18) = happyShift action_6
action_50 (19) = happyShift action_7
action_50 (22) = happyShift action_8
action_50 (23) = happyShift action_9
action_50 (27) = happyShift action_10
action_50 (30) = happyShift action_11
action_50 (32) = happyShift action_12
action_50 (4) = happyGoto action_55
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (6) = happyShift action_2
action_51 (13) = happyShift action_4
action_51 (17) = happyShift action_5
action_51 (18) = happyShift action_6
action_51 (19) = happyShift action_7
action_51 (22) = happyShift action_8
action_51 (23) = happyShift action_9
action_51 (27) = happyShift action_10
action_51 (30) = happyShift action_11
action_51 (32) = happyShift action_12
action_51 (4) = happyGoto action_54
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (25) = happyShift action_53
action_52 _ = happyReduce_16

action_53 (6) = happyShift action_59
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (6) = happyShift action_2
action_54 (8) = happyShift action_20
action_54 (9) = happyShift action_21
action_54 (10) = happyShift action_22
action_54 (11) = happyShift action_23
action_54 (12) = happyShift action_24
action_54 (13) = happyShift action_4
action_54 (14) = happyShift action_25
action_54 (15) = happyShift action_26
action_54 (16) = happyShift action_27
action_54 (17) = happyShift action_5
action_54 (18) = happyShift action_6
action_54 (19) = happyShift action_7
action_54 (22) = happyShift action_8
action_54 (23) = happyShift action_9
action_54 (27) = happyShift action_10
action_54 (30) = happyShift action_11
action_54 (32) = happyShift action_12
action_54 (4) = happyGoto action_19
action_54 _ = happyReduce_13

action_55 (6) = happyShift action_2
action_55 (8) = happyShift action_20
action_55 (9) = happyShift action_21
action_55 (10) = happyShift action_22
action_55 (11) = happyShift action_23
action_55 (12) = happyShift action_24
action_55 (13) = happyShift action_4
action_55 (14) = happyShift action_25
action_55 (15) = happyShift action_26
action_55 (16) = happyShift action_27
action_55 (17) = happyShift action_5
action_55 (18) = happyShift action_6
action_55 (19) = happyShift action_7
action_55 (22) = happyShift action_8
action_55 (23) = happyShift action_9
action_55 (27) = happyShift action_10
action_55 (30) = happyShift action_11
action_55 (32) = happyShift action_12
action_55 (4) = happyGoto action_19
action_55 _ = happyReduce_12

action_56 (30) = happyShift action_44
action_56 (34) = happyShift action_45
action_56 (35) = happyShift action_46
action_56 (5) = happyGoto action_58
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (6) = happyShift action_2
action_57 (8) = happyShift action_20
action_57 (9) = happyShift action_21
action_57 (10) = happyShift action_22
action_57 (11) = happyShift action_23
action_57 (12) = happyShift action_24
action_57 (13) = happyShift action_4
action_57 (14) = happyShift action_25
action_57 (15) = happyShift action_26
action_57 (16) = happyShift action_27
action_57 (17) = happyShift action_5
action_57 (18) = happyShift action_6
action_57 (19) = happyFail []
action_57 (22) = happyShift action_8
action_57 (23) = happyShift action_9
action_57 (27) = happyShift action_10
action_57 (30) = happyShift action_11
action_57 (32) = happyShift action_12
action_57 (4) = happyGoto action_19
action_57 _ = happyReduce_11

action_58 (31) = happyShift action_60
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_17

action_60 _ = happyReduce_23

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn4
		 (Num happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (BFalse
	)

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn4
		 (BTrue
	)

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or  happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Not happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 6 4 happyReduction_11
happyReduction_11 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 6 4 happyReduction_12
happyReduction_12 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 6 4 happyReduction_13
happyReduction_13 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_2  4 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  4 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Paren happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 4 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Pair 3 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 7 4 happyReduction_17
happyReduction_17 ((HappyTerminal (TokenNum happy_var_7)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Pair happy_var_7 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  4 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  4 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Geq happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  4 happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gth happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  5 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn5
		 (TBool
	)

happyReduce_22 = happySpecReduce_1  5 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn5
		 (TNum
	)

happyReduce_23 = happyReduce 5 5 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (TFun happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 36 36 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 6;
	TokenArrow -> cont 7;
	TokenAdd -> cont 8;
	TokenSub -> cont 9;
	TokenMul -> cont 10;
	TokenAnd -> cont 11;
	TokenOr -> cont 12;
	TokenNot -> cont 13;
	TokenEq -> cont 14;
	TokenGeq -> cont 15;
	TokenGth -> cont 16;
	TokenTrue -> cont 17;
	TokenFalse -> cont 18;
	TokenIf -> cont 19;
	TokenThen -> cont 20;
	TokenElse -> cont 21;
	TokenVar happy_dollar_dollar -> cont 22;
	TokenLam -> cont 23;
	TokenColon -> cont 24;
	TokenDot -> cont 25;
	TokenComma -> cont 26;
	TokenLet -> cont 27;
	TokenAss -> cont 28;
	TokenIn -> cont 29;
	TokenLParen -> cont 30;
	TokenRParen -> cont 31;
	TokenLCurlyB -> cont 32;
	TokenRCurlyB -> cont 33;
	TokenBoolean -> cont 34;
	TokenNumber -> cont 35;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 36 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
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





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
