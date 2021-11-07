{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
module Parse where

import CTL
import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

newtype HappyAbsSyn t4 t6 t8 t9 t11 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: t4 -> (HappyAbsSyn t4 t6 t8 t9 t11)
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn t4 t6 t8 t9 t11) -> t4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 (List State)
happyIn5 :: (List State) -> (HappyAbsSyn t4 t6 t8 t9 t11)
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn t4 t6 t8 t9 t11) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: t6 -> (HappyAbsSyn t4 t6 t8 t9 t11)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t4 t6 t8 t9 t11) -> t6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 (List Valuation)
happyIn7 :: (List Valuation) -> (HappyAbsSyn t4 t6 t8 t9 t11)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t4 t6 t8 t9 t11) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: t8 -> (HappyAbsSyn t4 t6 t8 t9 t11)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t4 t6 t8 t9 t11) -> t8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: t9 -> (HappyAbsSyn t4 t6 t8 t9 t11)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t4 t6 t8 t9 t11) -> t9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 (List Relation)
happyIn10 :: (List Relation) -> (HappyAbsSyn t4 t6 t8 t9 t11)
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn t4 t6 t8 t9 t11) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: t11 -> (HappyAbsSyn t4 t6 t8 t9 t11)
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn t4 t6 t8 t9 t11) -> t11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (CTL)
happyIn12 :: (CTL) -> (HappyAbsSyn t4 t6 t8 t9 t11)
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn t4 t6 t8 t9 t11) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyInTok :: (Token) -> (HappyAbsSyn t4 t6 t8 t9 t11)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t4 t6 t8 t9 t11) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_func","sts","states","vals","valuations","valuation","rels","relations","relation","ctl","AT","BT","TOP","NOT","'&'","'|'","THEN","EU","AX","EX","A","E","U","AF","EF","AG","EG","'('","')'","'['","']'","','","state","%eof"]
        bit_start = st Prelude.* 36
        bit_end = (st Prelude.+ 1) Prelude.* 36
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..35]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xee\xff\xf0\xff\xef\xff\xed\xff\xec\xff\xf2\xff\x00\x00\xf3\xff\xf1\xff\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x0a\x00\x00\x00\x0b\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\xfc\xff\x00\x00\xfe\xff\xfb\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x15\x00\x14\x00\x17\x00\x14\x00\x18\x00\x17\x00\x15\x00\x17\x00\x16\x00\x00\x00\xff\xff\x01\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x07\x00\x05\x00\x08\x00\x03\x00\xff\xff\x08\x00\x0a\x00\x08\x00\x09\x00\x03\x00\x00\x00\x05\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 28) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28)
	]

happy_n_terms = 25 :: Prelude.Int
happy_n_nonterms = 9 :: Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happySpecReduce_3  0# happyReduction_1
happyReduction_1 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn4
		 (happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happySpecReduce_2  0# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  happyIn4
		 (Nil
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TState happy_var_1) -> 
	happyIn5
		 (Cons happy_var_1 Nil
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happySpecReduce_3  1# happyReduction_4
happyReduction_4 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TState happy_var_1) -> 
	case happyOut5 happy_x_3 of { (HappyWrap5 happy_var_3) -> 
	happyIn5
		 (Cons happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happySpecReduce_3  2# happyReduction_5
happyReduction_5 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
	happyIn6
		 (happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happySpecReduce_1  3# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (Cons happy_var_1 Nil
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happySpecReduce_3  3# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	happyIn7
		 (Cons happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happyReduce 5# 4# happyReduction_8
happyReduction_8 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TAt happy_var_2) -> 
	case happyOut4 happy_x_4 of { happy_var_4 -> 
	happyIn8
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_3  5# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn9
		 (happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_1  6# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (Cons happy_var_1 Nil
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happySpecReduce_3  6# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn10
		 (Cons happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happyReduce 5# 7# happyReduction_12
happyReduction_12 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TState happy_var_2) -> 
	case happyOutTok happy_x_4 of { (TState happy_var_4) -> 
	happyIn11
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happySpecReduce_1  8# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TAt happy_var_1) -> 
	happyIn12
		 (Atomic  happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happySpecReduce_1  8# happyReduction_14
happyReduction_14 happy_x_1
	 =  happyIn12
		 (Bottom
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happySpecReduce_1  8# happyReduction_15
happyReduction_15 happy_x_1
	 =  happyIn12
		 (Top
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happySpecReduce_2  8# happyReduction_16
happyReduction_16 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn12
		 (Not happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_3  8# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 (And happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_3  8# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 (Or happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_3  8# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 (Then happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_2  8# happyReduction_20
happyReduction_20 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn12
		 (AX happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_2  8# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn12
		 (EX happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happyReduce 6# 8# happyReduction_22
happyReduction_22 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut12 happy_x_5 of { (HappyWrap12 happy_var_5) -> 
	happyIn12
		 (AU happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happySpecReduce_3  8# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 (EU happy_var_2 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happySpecReduce_2  8# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn12
		 (AF happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happySpecReduce_2  8# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn12
		 (EF happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_26 = happySpecReduce_2  8# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn12
		 (AG happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_27 = happySpecReduce_2  8# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn12
		 (EG happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_28 = happySpecReduce_3  8# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn12
		 (Parens happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 24# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TAt happy_dollar_dollar -> cont 1#;
	TBT -> cont 2#;
	TTop -> cont 3#;
	TNot -> cont 4#;
	TAnd -> cont 5#;
	TOr -> cont 6#;
	TThen -> cont 7#;
	TEu -> cont 8#;
	TAx -> cont 9#;
	TEx -> cont 10#;
	TAll -> cont 11#;
	TExists -> cont 12#;
	TUntil -> cont 13#;
	TAf -> cont 14#;
	TEf -> cont 15#;
	TAg -> cont 16#;
	TEg -> cont 17#;
	TParenLeft -> cont 18#;
	TParenRight -> cont 19#;
	TLBracket -> cont 20#;
	TRBracket -> cont 21#;
	TComma -> cont 22#;
	TState happy_dollar_dollar -> cont 23#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 24# tk tks = happyError' (tks, explist)
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
func tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {x' = happyOut4 x} in x'))

happySeq = happyDontSeq


data Token = TAt String
            | TBT 
            | TTop
            | TNot
            | TAnd
            | TOr
            | TThen
            | TAx
            | TEx
            | TAll
            | TExists
            | TUntil
            | TEu
            | TAf
            | TEf
            | TAg
            | TEg
            | TParenLeft
            | TParenRight 
            | TLBracket
            | TRBracket
            | TComma
            | TState String

            deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- TODO: fijarse de hacer otro lexer auxiliar y ver donde llamarlo

-- Propositions

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
lexer ('&':cs) = TAnd : lexer cs
lexer ('|':cs) = TOr : lexer cs
lexer ('(':cs) = TParenLeft : lexer cs
lexer (')':cs) = TParenRight : lexer cs
lexer ('[':cs) = TLBracket : lexer cs
lexer (']':cs) = TRBracket : lexer cs

lexVar :: String -> [Token]
lexVar cs = case span isAlpha cs of
                ("BT", rest) -> TBT : lexer rest
                ("TOP", rest) -> TTop : lexer rest
                ("NOT", rest) -> TNot : lexer rest
                ("THEN", rest) -> TThen : lexer rest
                ("AX", rest) -> TAx : lexer rest
                ("EX", rest) -> TEx : lexer rest
                ("AF", rest) -> TAf : lexer rest
                ("EF", rest) -> TEf : lexer rest
                ("AG", rest) -> TAg : lexer rest
                ("EG", rest) -> TEg : lexer rest
                ("EU", rest) -> TEu : lexer rest
                (var, rest) -> (TAt var) : lexer rest

-- States

lexer4states :: String -> [Token]
lexer4states [] = []
lexer4states (',':cs) = TComma : lexer4states cs
lexer4states ('[':cs) = TLBracket : lexer4states cs
lexer4states (']':cs) = TRBracket : lexer4states cs
lexer4states (c:cs)
             | isSpace c = lexer4states cs
             | isAlpha c = lexState (c:cs)

lexState :: String -> [Token]
lexState [] = []
lexState cs = case span isAlpha cs of
                (var, rest) -> (TState var) : lexer4states rest

-- Relations

lexer4relations :: String -> [Token]
lexer4relations [] = []
lexer4relations ('[':cs) = TLBracket : lexer4relations cs
lexer4relations (']':cs) = TRBracket : lexer4relations cs
lexer4relations (',':cs) = TComma : lexer4relations cs
lexer4relations ('(':cs) = TParenLeft : lexRelation cs
lexer4relations (c:cs) | isSpace c = lexer4relations cs


lexRelation :: String -> [Token]
lexRelation [] = []
lexRelation (')':cs) = TParenRight : lexer4relations cs
lexRelation (',':cs) = TComma : lexRelation cs
lexRelation css@(c:cs)
                | isSpace c = lexRelation cs
                | isAlpha c = case span isAlpha css of
                                (var, rest) -> (TState var) : lexRelation rest

-- Valuations

lexer4valuations :: String -> [Token]
lexer4valuations [] = []
lexer4valuations ('[':cs) = TLBracket : lexer4valuations cs
lexer4valuations (']':cs) = TRBracket : lexer4valuations cs
lexer4valuations (',':cs) = TComma : lexer4valuations cs
lexer4valuations ('(': cs) = TParenLeft : lexValuation cs
lexer4valuations (c:cs) | isSpace c = lexer4valuations cs

lexValuation :: String -> [Token]
lexValuation [] = []
lexValuation (')':cs) = TParenRight : lexer4valuations cs
lexValuation (',':cs) = TComma : lexValuation cs
lexValuation ('[':cs) = valuationStates cs
lexValuation css@(c:cs)
                    | isSpace c = lexValuation cs
                    | isAlpha c = case span isAlpha css of
                        (var, rest) -> (TAt var) : lexValuation rest 

-- FIXME: codigo repetidooooooooooooooooooooooo aaaaaaaaaaaaaaaaaaaaaa

valuationStates :: String -> [Token]
valuationStates [] = []
valuationStates (',':cs) = TComma : valuationStates cs
valuationStates (']':cs) = TRBracket : lexValuation cs
valuationStates (c:cs)
             | isSpace c = valuationStates cs
             | isAlpha c = valuationState (c:cs)

valuationState :: String -> [Token]
valuationState [] = []
valuationState cs = case span isAlpha cs of
                        (var, rest) -> (TState var) : valuationStates rest
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































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
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
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
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


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


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
