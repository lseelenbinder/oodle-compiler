{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module Oodle.Parser where

import Oodle.Token
import qualified Data.Array as Oodle_Data_Array
import qualified GHC.Exts as Oodle_GHC_Exts

-- parser produced by Oodle Version 1.19.3

newtype OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30 = OodleAbsSyn OodleAny
#if __GLASGOW_HASKELL__ >= 607
type OodleAny = Oodle_GHC_Exts.Any
#else
type OodleAny = forall a . a
#endif
oodleIn4 :: t4 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn4 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn4 #-}
oodleOut4 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t4
oodleOut4 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut4 #-}
oodleIn5 :: ([Class]) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn5 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn5 #-}
oodleOut5 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> ([Class])
oodleOut5 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut5 #-}
oodleIn6 :: t6 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn6 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn6 #-}
oodleOut6 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t6
oodleOut6 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut6 #-}
oodleIn7 :: (Id) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn7 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn7 #-}
oodleOut7 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> (Id)
oodleOut7 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut7 #-}
oodleIn8 :: ([Method]) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn8 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn8 #-}
oodleOut8 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> ([Method])
oodleOut8 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut8 #-}
oodleIn9 :: t9 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn9 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn9 #-}
oodleOut9 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t9
oodleOut9 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut9 #-}
oodleIn10 :: ([Var]) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn10 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn10 #-}
oodleOut10 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> ([Var])
oodleOut10 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut10 #-}
oodleIn11 :: t11 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn11 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn11 #-}
oodleOut11 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t11
oodleOut11 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut11 #-}
oodleIn12 :: t12 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn12 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn12 #-}
oodleOut12 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t12
oodleOut12 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut12 #-}
oodleIn13 :: t13 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn13 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn13 #-}
oodleOut13 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t13
oodleOut13 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut13 #-}
oodleIn14 :: ([Argument]) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn14 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn14 #-}
oodleOut14 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> ([Argument])
oodleOut14 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut14 #-}
oodleIn15 :: t15 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn15 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn15 #-}
oodleOut15 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t15
oodleOut15 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut15 #-}
oodleIn16 :: ([Statement]) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn16 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn16 #-}
oodleOut16 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> ([Statement])
oodleOut16 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut16 #-}
oodleIn17 :: (Statement) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn17 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn17 #-}
oodleOut17 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> (Statement)
oodleOut17 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut17 #-}
oodleIn18 :: (Statement) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn18 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn18 #-}
oodleOut18 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> (Statement)
oodleOut18 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut18 #-}
oodleIn19 :: (Statement) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn19 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn19 #-}
oodleOut19 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> (Statement)
oodleOut19 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut19 #-}
oodleIn20 :: ([Statement]) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn20 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn20 #-}
oodleOut20 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> ([Statement])
oodleOut20 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut20 #-}
oodleIn21 :: (Statement) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn21 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn21 #-}
oodleOut21 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> (Statement)
oodleOut21 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut21 #-}
oodleIn22 :: (Statement) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn22 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn22 #-}
oodleOut22 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> (Statement)
oodleOut22 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut22 #-}
oodleIn23 :: ((Expression, Id)) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn23 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn23 #-}
oodleOut23 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> ((Expression, Id))
oodleOut23 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut23 #-}
oodleIn24 :: ([Expression]) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn24 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn24 #-}
oodleOut24 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> ([Expression])
oodleOut24 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut24 #-}
oodleIn25 :: t25 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn25 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn25 #-}
oodleOut25 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t25
oodleOut25 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut25 #-}
oodleIn26 :: t26 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn26 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn26 #-}
oodleOut26 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t26
oodleOut26 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut26 #-}
oodleIn27 :: t27 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn27 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn27 #-}
oodleOut27 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t27
oodleOut27 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut27 #-}
oodleIn28 :: (Expression) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn28 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn28 #-}
oodleOut28 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> (Expression)
oodleOut28 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut28 #-}
oodleIn29 :: ([Expression]) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn29 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn29 #-}
oodleOut29 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> ([Expression])
oodleOut29 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut29 #-}
oodleIn30 :: t30 -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleIn30 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleIn30 #-}
oodleOut30 :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> t30
oodleOut30 x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOut30 #-}
oodleInTok :: (Token) -> (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30)
oodleInTok x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleInTok #-}
oodleOutTok :: (OodleAbsSyn t4 t6 t9 t11 t12 t13 t15 t25 t26 t27 t30) -> (Token)
oodleOutTok x = Oodle_GHC_Exts.unsafeCoerce# x
{-# INLINE oodleOutTok #-}


oodleActOffsets :: OodleAddr
oodleActOffsets = OodleA# "\x73\x01\x73\x01\x50\x00\x00\x00\x47\x01\x00\x00\x6e\x01\x00\x00\x6d\x01\x6c\x01\x00\x00\x69\x01\x67\x01\x6b\x01\x68\x01\x66\x01\x00\x00\xf0\x00\x6a\x01\xd7\x00\x00\x00\x06\x00\x00\x00\x4d\x01\x64\x01\x34\x00\x00\x00\x48\x01\x65\x01\x63\x01\x45\x01\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x43\x01\x62\x01\x39\x00\x41\x01\xb7\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x00\x00\x34\x00\x39\x00\x00\x00\x00\x00\x39\x00\x39\x00\x39\x00\x5f\x01\x34\x00\x40\x01\x5b\x01\x00\x00\x39\x00\x7e\x00\x00\x00\x4f\x01\x3c\x01\x6b\x00\x32\x01\x32\x01\x32\x01\x3b\x01\x3b\x01\x39\x00\x39\x00\x39\x00\x39\x00\x39\x00\x39\x00\x39\x00\x39\x00\x39\x00\x39\x00\x58\x01\x39\x00\x35\x01\x58\x00\x00\x00\x00\x00\xe1\x00\xe1\x00\xe1\x00\x30\x01\x30\x01\x13\x00\x13\x00\x3d\x00\xc8\x00\xd0\x00\x00\x00\x33\x01\x00\x00\x56\x01\x00\x00\x55\x01\x00\x00\x39\x00\x00\x00\x00\x00\x68\x00\x2e\x01\x53\x01\x23\x00\x4c\x01\x52\x01\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x01\xb7\x00\x45\x00\x39\x00\x36\x01\x39\x00\xeb\xff\x2f\x01\x2b\x01\x39\x00\x23\x00\x49\x01\x4b\x01\x00\x00\x2a\x01\xa4\x00\x00\x00\x4a\x01\x01\x00\x23\x00\x23\x00\x91\x00\x46\x01\x42\x01\x3f\x01\x39\x01\x3d\x01\x44\x01\x23\x00\x38\x01\x00\x00\x00\x00\x00\x00\x00\x00"#

oodleGotoOffsets :: OodleAddr
oodleGotoOffsets = OodleA# "\x0e\x00\x29\x01\xf5\x00\x00\x00\x00\x00\x00\x00\xc9\x00\x00\x00\x00\x00\x3e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x28\x01\x3a\x01\x00\x00\x2d\x01\x00\x00\x8d\x00\x00\x00\x37\x01\x00\x00\x34\x01\x74\x00\x27\x01\x00\x00\x00\x00\x19\x01\x00\x00\x26\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\x00\xe2\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\x00\x00\x00\x00\x00\xf6\x00\xc7\x00\x00\x00\x00\x00\xb6\x00\xb5\x00\xb4\x00\x00\x00\xf1\x00\xd5\x00\xac\x00\x00\x00\xa3\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x25\x01\xa2\x00\xa1\x00\x90\x00\x8f\x00\x8e\x00\x7c\x00\x7b\x00\x6a\x00\x69\x00\x57\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x01\x00\x00\x9e\x00\x00\x00\x75\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x9d\x00\x65\x00\x3e\x00\x1d\x01\x00\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x01\x2e\x00\x00\x00\x16\x00\x00\x00\x46\x00\xf9\x00\xfc\xff\x15\x01\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\xf7\xff\x0d\x01\x05\x01\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xf8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

oodleDefActions :: OodleAddr
oodleDefActions = OodleA# "\x00\x00\x00\x00\x00\x00\xb8\xff\x00\x00\xfe\xff\xfd\xff\xb7\xff\x00\x00\xf9\xff\xfc\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\xfa\xff\x00\x00\x00\x00\x00\x00\xf5\xff\xf1\xff\xf7\xff\xef\xff\xec\xff\x00\x00\xf8\xff\x00\x00\x00\x00\xfb\xff\xf2\xff\xd4\xff\xd5\xff\xd7\xff\xd6\xff\x00\x00\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\xd0\xff\xd1\xff\xd2\xff\xce\xff\xcc\xff\x00\x00\x00\x00\xcd\xff\xcf\xff\x00\x00\x00\x00\x00\x00\xf3\xff\x00\x00\xf1\xff\x00\x00\xd3\xff\x00\x00\x00\x00\xee\xff\x00\x00\xeb\xff\x00\x00\xc7\xff\xc6\xff\xc8\xff\xcb\xff\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\x00\x00\xba\xff\xdc\xff\xde\xff\xc0\xff\xbe\xff\xbf\xff\xc4\xff\xc5\xff\xc2\xff\xc3\xff\xc1\xff\xbc\xff\xbd\xff\xbb\xff\xda\xff\xca\xff\x00\x00\xd8\xff\xf4\xff\xd9\xff\x00\x00\xc9\xff\xb9\xff\x00\x00\xf1\xff\x00\x00\xea\xff\x00\x00\x00\x00\xe8\xff\xe7\xff\xe6\xff\xe5\xff\x00\x00\x00\x00\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\xda\xff\xdb\xff\xea\xff\x00\x00\x00\x00\xe9\xff\x00\x00\xd9\xff\xe4\xff\x00\x00\x00\x00\xea\xff\xea\xff\xdf\xff\xf6\xff\xe1\xff\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\x00\x00\xe0\xff\xe3\xff\xe2\xff"#

oodleCheck :: OodleAddr
oodleCheck = OodleA# "\xff\xff\x16\x00\x01\x00\x1a\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x13\x00\x14\x00\x1a\x00\x13\x00\x14\x00\x18\x00\x19\x00\x2b\x00\x18\x00\x19\x00\x10\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1a\x00\x13\x00\x24\x00\x1a\x00\x2b\x00\x0a\x00\x18\x00\x0c\x00\x2a\x00\x1e\x00\x1f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x04\x00\x05\x00\x17\x00\x02\x00\x03\x00\x04\x00\x2b\x00\x1c\x00\x1d\x00\x13\x00\x0e\x00\x0a\x00\x25\x00\x1a\x00\x18\x00\x24\x00\x29\x00\x15\x00\x11\x00\x12\x00\x13\x00\x14\x00\x13\x00\x09\x00\x17\x00\x01\x00\x1a\x00\x18\x00\x19\x00\x1c\x00\x1d\x00\x07\x00\x1a\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x24\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x2b\x00\x24\x00\x13\x00\x26\x00\x04\x00\x08\x00\x06\x00\x18\x00\x2b\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x06\x00\x13\x00\x13\x00\x0a\x00\x0b\x00\x28\x00\x18\x00\x18\x00\x2b\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x13\x00\x13\x00\x25\x00\x04\x00\x05\x00\x18\x00\x18\x00\x16\x00\x2b\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x13\x00\x13\x00\x13\x00\x07\x00\x27\x00\x18\x00\x18\x00\x18\x00\x2b\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x13\x00\x13\x00\x13\x00\x0b\x00\x1a\x00\x18\x00\x18\x00\x18\x00\x2b\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x13\x00\x13\x00\x13\x00\x01\x00\x02\x00\x18\x00\x18\x00\x18\x00\x2b\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x13\x00\x04\x00\x24\x00\x08\x00\x26\x00\x18\x00\x09\x00\x19\x00\x2b\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x2b\x00\x04\x00\x13\x00\x01\x00\x02\x00\x1a\x00\x09\x00\x18\x00\x2b\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x17\x00\x11\x00\x12\x00\x13\x00\x2b\x00\x17\x00\x15\x00\x16\x00\x18\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x16\x00\x11\x00\x12\x00\x13\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x18\x00\x11\x00\x12\x00\x13\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x18\x00\x11\x00\x12\x00\x13\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x18\x00\x11\x00\x12\x00\x13\x00\x04\x00\x05\x00\x1a\x00\x07\x00\x18\x00\x15\x00\x16\x00\x15\x00\x16\x00\x15\x00\x16\x00\x16\x00\x09\x00\x17\x00\x08\x00\x06\x00\x03\x00\x1a\x00\x1a\x00\x0c\x00\x01\x00\x09\x00\x01\x00\x09\x00\x10\x00\x08\x00\x01\x00\x01\x00\x04\x00\x18\x00\x25\x00\x24\x00\x26\x00\x23\x00\x01\x00\x01\x00\x09\x00\x01\x00\x01\x00\x2a\x00\x26\x00\x25\x00\x2b\x00\x04\x00\x2b\x00\x0f\x00\x04\x00\x01\x00\x26\x00\x26\x00\x01\x00\x01\x00\x24\x00\x01\x00\x01\x00\x04\x00\x01\x00\x2a\x00\x26\x00\x24\x00\x2a\x00\x04\x00\x04\x00\x23\x00\x04\x00\x0b\x00\x2c\x00\x01\x00\x07\x00\xff\xff\xff\xff\x0f\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

oodleTable :: OodleAddr
oodleTable = OodleA# "\x00\x00\x86\x00\x04\x00\x90\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x04\x00\x28\x00\x82\x00\x87\x00\x28\x00\x52\x00\x53\x00\x54\x00\x51\x00\x53\x00\x54\x00\x8e\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x08\x00\x2b\x00\x2c\x00\x77\x00\x02\x00\x28\x00\x19\x00\x88\x00\x51\x00\x2e\x00\x86\x00\x78\x00\x1a\x00\x4c\x00\x4d\x00\x79\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x20\x00\x21\x00\x33\x00\x2b\x00\x2c\x00\x2d\x00\x51\x00\x34\x00\x35\x00\x28\x00\x22\x00\x2e\x00\x39\x00\x8a\x00\x7a\x00\x36\x00\x3a\x00\x23\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x28\x00\x84\x00\x33\x00\x08\x00\x7e\x00\x53\x00\x69\x00\x34\x00\x35\x00\x09\x00\x6d\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x36\x00\xd2\xff\xd2\xff\xd2\xff\xd2\xff\xd2\xff\xd2\xff\xd2\xff\xd2\xff\xd2\xff\xd2\xff\x51\x00\xdd\xff\x28\x00\x3c\x00\x6c\x00\x17\x00\x6d\x00\x56\x00\xd2\xff\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x6a\x00\x28\x00\x28\x00\x23\x00\x24\x00\x68\x00\x57\x00\x58\x00\x51\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x28\x00\x28\x00\x63\x00\x1a\x00\x13\x00\x59\x00\x5a\x00\x3a\x00\x51\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x28\x00\x28\x00\x28\x00\x14\x00\x65\x00\x5b\x00\x5c\x00\x5d\x00\x51\x00\xc9\xff\xc9\xff\xc9\xff\xc9\xff\xc9\xff\xc9\xff\xc9\xff\xc9\xff\xc9\xff\xc9\xff\x28\x00\x28\x00\x28\x00\x3d\x00\x65\x00\x5e\x00\x5f\x00\x3c\x00\xc9\xff\xbb\xff\xbb\xff\xbb\xff\xbb\xff\xbb\xff\xbb\xff\xbb\xff\xbb\xff\xbb\xff\xbb\xff\x28\x00\x28\x00\x28\x00\x0a\x00\x06\x00\x40\x00\x41\x00\x42\x00\xbb\xff\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x28\x00\x1c\x00\xdd\xff\x3e\x00\x3c\x00\x43\x00\x17\x00\x47\x00\x51\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x16\x00\x28\x00\x05\x00\x06\x00\x36\x00\x17\x00\x29\x00\x51\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x94\x00\x6f\x00\x70\x00\x71\x00\x3f\x00\x72\x00\x73\x00\x74\x00\x51\x00\x44\x00\x83\x00\x61\x00\x75\x00\x8b\x00\x6f\x00\x70\x00\x71\x00\x45\x00\x72\x00\x73\x00\x74\x00\x8c\x00\x6f\x00\x70\x00\x71\x00\x75\x00\x72\x00\x73\x00\x74\x00\x81\x00\x6f\x00\x70\x00\x71\x00\x75\x00\x72\x00\x73\x00\x74\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x75\x00\x72\x00\x73\x00\x74\x00\x12\x00\x13\x00\x1d\x00\x14\x00\x75\x00\x7b\x00\x7c\x00\x66\x00\x61\x00\x60\x00\x61\x00\x3a\x00\x26\x00\x1e\x00\x17\x00\x11\x00\x0b\x00\x0f\x00\x02\x00\x94\x00\x04\x00\x92\x00\x08\x00\x8e\x00\x93\x00\x90\x00\x04\x00\x04\x00\x81\x00\x7a\x00\x8a\x00\x7e\x00\x3c\x00\x28\x00\x04\x00\x04\x00\x80\x00\x08\x00\x04\x00\x1a\x00\x3c\x00\x69\x00\x51\x00\x56\x00\x51\x00\x64\x00\x26\x00\x08\x00\x3c\x00\x3c\x00\x04\x00\x08\x00\x52\x00\x04\x00\x08\x00\x26\x00\x04\x00\x1a\x00\x3c\x00\x19\x00\x38\x00\x1d\x00\x11\x00\x28\x00\x0a\x00\x0e\x00\xff\xff\x04\x00\x09\x00\x00\x00\x00\x00\x0f\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

oodleReduceArr = Oodle_Data_Array.array (1, 72) [
	(1 , oodleReduce_1),
	(2 , oodleReduce_2),
	(3 , oodleReduce_3),
	(4 , oodleReduce_4),
	(5 , oodleReduce_5),
	(6 , oodleReduce_6),
	(7 , oodleReduce_7),
	(8 , oodleReduce_8),
	(9 , oodleReduce_9),
	(10 , oodleReduce_10),
	(11 , oodleReduce_11),
	(12 , oodleReduce_12),
	(13 , oodleReduce_13),
	(14 , oodleReduce_14),
	(15 , oodleReduce_15),
	(16 , oodleReduce_16),
	(17 , oodleReduce_17),
	(18 , oodleReduce_18),
	(19 , oodleReduce_19),
	(20 , oodleReduce_20),
	(21 , oodleReduce_21),
	(22 , oodleReduce_22),
	(23 , oodleReduce_23),
	(24 , oodleReduce_24),
	(25 , oodleReduce_25),
	(26 , oodleReduce_26),
	(27 , oodleReduce_27),
	(28 , oodleReduce_28),
	(29 , oodleReduce_29),
	(30 , oodleReduce_30),
	(31 , oodleReduce_31),
	(32 , oodleReduce_32),
	(33 , oodleReduce_33),
	(34 , oodleReduce_34),
	(35 , oodleReduce_35),
	(36 , oodleReduce_36),
	(37 , oodleReduce_37),
	(38 , oodleReduce_38),
	(39 , oodleReduce_39),
	(40 , oodleReduce_40),
	(41 , oodleReduce_41),
	(42 , oodleReduce_42),
	(43 , oodleReduce_43),
	(44 , oodleReduce_44),
	(45 , oodleReduce_45),
	(46 , oodleReduce_46),
	(47 , oodleReduce_47),
	(48 , oodleReduce_48),
	(49 , oodleReduce_49),
	(50 , oodleReduce_50),
	(51 , oodleReduce_51),
	(52 , oodleReduce_52),
	(53 , oodleReduce_53),
	(54 , oodleReduce_54),
	(55 , oodleReduce_55),
	(56 , oodleReduce_56),
	(57 , oodleReduce_57),
	(58 , oodleReduce_58),
	(59 , oodleReduce_59),
	(60 , oodleReduce_60),
	(61 , oodleReduce_61),
	(62 , oodleReduce_62),
	(63 , oodleReduce_63),
	(64 , oodleReduce_64),
	(65 , oodleReduce_65),
	(66 , oodleReduce_66),
	(67 , oodleReduce_67),
	(68 , oodleReduce_68),
	(69 , oodleReduce_69),
	(70 , oodleReduce_70),
	(71 , oodleReduce_71),
	(72 , oodleReduce_72)
	]

oodle_n_terms = 45 :: Int
oodle_n_nonterms = 27 :: Int

oodleReduce_1 = oodleSpecReduce_2  0# oodleReduction_1
oodleReduction_1 oodle_x_2
	oodle_x_1
	 =  case oodleOut5 oodle_x_2 of { oodle_var_2 -> 
	oodleIn4
		 (Start oodle_var_2
	)}

oodleReduce_2 = oodleSpecReduce_1  1# oodleReduction_2
oodleReduction_2 oodle_x_1
	 =  case oodleOut6 oodle_x_1 of { oodle_var_1 -> 
	oodleIn5
		 ([oodle_var_1]
	)}

oodleReduce_3 = oodleSpecReduce_2  1# oodleReduction_3
oodleReduction_3 oodle_x_2
	oodle_x_1
	 =  case oodleOut6 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut5 oodle_x_2 of { oodle_var_2 -> 
	oodleIn5
		 (oodle_var_1 : oodle_var_2
	)}}

oodleReduce_4 = oodleReduce 9# 2# oodleReduction_4
oodleReduction_4 (oodle_x_9 `OodleStk`
	oodle_x_8 `OodleStk`
	oodle_x_7 `OodleStk`
	oodle_x_6 `OodleStk`
	oodle_x_5 `OodleStk`
	oodle_x_4 `OodleStk`
	oodle_x_3 `OodleStk`
	oodle_x_2 `OodleStk`
	oodle_x_1 `OodleStk`
	oodleRest)
	 = case oodleOutTok oodle_x_2 of { (Token (TokenIdentifier oodle_var_2) _) -> 
	case oodleOut7 oodle_x_3 of { oodle_var_3 -> 
	case oodleOut10 oodle_x_6 of { oodle_var_6 -> 
	case oodleOut8 oodle_x_7 of { oodle_var_7 -> 
	case oodleOutTok oodle_x_8 of { (Token (TokenIdentifier oodle_var_8) _) -> 
	oodleIn6
		 (Class (Id oodle_var_2) oodle_var_3 oodle_var_6 oodle_var_7 (Id oodle_var_8)
	) `OodleStk` oodleRest}}}}}

oodleReduce_5 = oodleSpecReduce_3  3# oodleReduction_5
oodleReduction_5 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOutTok oodle_x_3 of { (Token (TokenIdentifier oodle_var_3) _) -> 
	oodleIn7
		 (Id oodle_var_3
	)}

oodleReduce_6 = oodleSpecReduce_0  3# oodleReduction_6
oodleReduction_6  =  oodleIn7
		 (Id ""
	)

oodleReduce_7 = oodleSpecReduce_2  4# oodleReduction_7
oodleReduction_7 oodle_x_2
	oodle_x_1
	 =  case oodleOut9 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut8 oodle_x_2 of { oodle_var_2 -> 
	oodleIn8
		 (oodle_var_1 : oodle_var_2
	)}}

oodleReduce_8 = oodleSpecReduce_1  4# oodleReduction_8
oodleReduction_8 oodle_x_1
	 =  oodleIn8
		 ([]
	)

oodleReduce_9 = oodleReduce 14# 5# oodleReduction_9
oodleReduction_9 (oodle_x_14 `OodleStk`
	oodle_x_13 `OodleStk`
	oodle_x_12 `OodleStk`
	oodle_x_11 `OodleStk`
	oodle_x_10 `OodleStk`
	oodle_x_9 `OodleStk`
	oodle_x_8 `OodleStk`
	oodle_x_7 `OodleStk`
	oodle_x_6 `OodleStk`
	oodle_x_5 `OodleStk`
	oodle_x_4 `OodleStk`
	oodle_x_3 `OodleStk`
	oodle_x_2 `OodleStk`
	oodle_x_1 `OodleStk`
	oodleRest)
	 = case oodleOutTok oodle_x_1 of { (Token (TokenIdentifier oodle_var_1) _) -> 
	case oodleOut14 oodle_x_3 of { oodle_var_3 -> 
	case oodleOut12 oodle_x_5 of { oodle_var_5 -> 
	case oodleOut10 oodle_x_8 of { oodle_var_8 -> 
	case oodleOut16 oodle_x_11 of { oodle_var_11 -> 
	case oodleOutTok oodle_x_13 of { (Token (TokenIdentifier oodle_var_13) _) -> 
	oodleIn9
		 (Method (Id oodle_var_1) oodle_var_5 oodle_var_3 oodle_var_8 oodle_var_11 (Id oodle_var_13)
	) `OodleStk` oodleRest}}}}}}

oodleReduce_10 = oodleSpecReduce_2  6# oodleReduction_10
oodleReduction_10 oodle_x_2
	oodle_x_1
	 =  case oodleOut10 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut11 oodle_x_2 of { oodle_var_2 -> 
	oodleIn10
		 ((concat [oodle_var_1, [oodle_var_2]])
	)}}

oodleReduce_11 = oodleSpecReduce_0  6# oodleReduction_11
oodleReduction_11  =  oodleIn10
		 ([] :: [Var]
	)

oodleReduce_12 = oodleReduce 4# 7# oodleReduction_12
oodleReduction_12 (oodle_x_4 `OodleStk`
	oodle_x_3 `OodleStk`
	oodle_x_2 `OodleStk`
	oodle_x_1 `OodleStk`
	oodleRest)
	 = case oodleOutTok oodle_x_1 of { (Token (TokenIdentifier oodle_var_1) _) -> 
	case oodleOut12 oodle_x_2 of { oodle_var_2 -> 
	case oodleOut13 oodle_x_3 of { oodle_var_3 -> 
	oodleIn11
		 (Var (Id oodle_var_1) oodle_var_2 oodle_var_3
	) `OodleStk` oodleRest}}}

oodleReduce_13 = oodleSpecReduce_2  8# oodleReduction_13
oodleReduction_13 oodle_x_2
	oodle_x_1
	 =  case oodleOut27 oodle_x_2 of { oodle_var_2 -> 
	oodleIn12
		 (oodle_var_2
	)}

oodleReduce_14 = oodleSpecReduce_0  8# oodleReduction_14
oodleReduction_14  =  oodleIn12
		 (TypeNull
	)

oodleReduce_15 = oodleSpecReduce_2  9# oodleReduction_15
oodleReduction_15 oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_2 of { oodle_var_2 -> 
	oodleIn13
		 (oodle_var_2
	)}

oodleReduce_16 = oodleSpecReduce_0  9# oodleReduction_16
oodleReduction_16  =  oodleIn13
		 (ExpressionNull
	)

oodleReduce_17 = oodleSpecReduce_3  10# oodleReduction_17
oodleReduction_17 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut14 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut15 oodle_x_3 of { oodle_var_3 -> 
	oodleIn14
		 (oodle_var_3 : oodle_var_1
	)}}

oodleReduce_18 = oodleSpecReduce_1  10# oodleReduction_18
oodleReduction_18 oodle_x_1
	 =  case oodleOut15 oodle_x_1 of { oodle_var_1 -> 
	oodleIn14
		 ([oodle_var_1]
	)}

oodleReduce_19 = oodleSpecReduce_0  10# oodleReduction_19
oodleReduction_19  =  oodleIn14
		 ([]
	)

oodleReduce_20 = oodleSpecReduce_3  11# oodleReduction_20
oodleReduction_20 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOutTok oodle_x_1 of { (Token (TokenIdentifier oodle_var_1) _) -> 
	case oodleOut27 oodle_x_3 of { oodle_var_3 -> 
	oodleIn15
		 (Argument (Id oodle_var_1) oodle_var_3
	)}}

oodleReduce_21 = oodleSpecReduce_0  12# oodleReduction_21
oodleReduction_21  =  oodleIn16
		 ([]
	)

oodleReduce_22 = oodleSpecReduce_3  12# oodleReduction_22
oodleReduction_22 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut17 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut16 oodle_x_3 of { oodle_var_3 -> 
	oodleIn16
		 (oodle_var_1 : oodle_var_3
	)}}

oodleReduce_23 = oodleSpecReduce_1  13# oodleReduction_23
oodleReduction_23 oodle_x_1
	 =  case oodleOut18 oodle_x_1 of { oodle_var_1 -> 
	oodleIn17
		 (oodle_var_1
	)}

oodleReduce_24 = oodleSpecReduce_1  13# oodleReduction_24
oodleReduction_24 oodle_x_1
	 =  case oodleOut19 oodle_x_1 of { oodle_var_1 -> 
	oodleIn17
		 (oodle_var_1
	)}

oodleReduce_25 = oodleSpecReduce_1  13# oodleReduction_25
oodleReduction_25 oodle_x_1
	 =  case oodleOut21 oodle_x_1 of { oodle_var_1 -> 
	oodleIn17
		 (oodle_var_1
	)}

oodleReduce_26 = oodleSpecReduce_1  13# oodleReduction_26
oodleReduction_26 oodle_x_1
	 =  case oodleOut22 oodle_x_1 of { oodle_var_1 -> 
	oodleIn17
		 (oodle_var_1
	)}

oodleReduce_27 = oodleSpecReduce_3  14# oodleReduction_27
oodleReduction_27 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOutTok oodle_x_1 of { (Token (TokenIdentifier oodle_var_1) _) -> 
	case oodleOut25 oodle_x_2 of { oodle_var_2 -> 
	case oodleOut13 oodle_x_3 of { oodle_var_3 -> 
	oodleIn18
		 (AssignStatement (IdArray oodle_var_1 oodle_var_2) oodle_var_3
	)}}}

oodleReduce_28 = oodleReduce 8# 15# oodleReduction_28
oodleReduction_28 (oodle_x_8 `OodleStk`
	oodle_x_7 `OodleStk`
	oodle_x_6 `OodleStk`
	oodle_x_5 `OodleStk`
	oodle_x_4 `OodleStk`
	oodle_x_3 `OodleStk`
	oodle_x_2 `OodleStk`
	oodle_x_1 `OodleStk`
	oodleRest)
	 = case oodleOut28 oodle_x_2 of { oodle_var_2 -> 
	case oodleOut16 oodle_x_5 of { oodle_var_5 -> 
	case oodleOut20 oodle_x_6 of { oodle_var_6 -> 
	oodleIn19
		 (IfStatement oodle_var_2 oodle_var_5 oodle_var_6
	) `OodleStk` oodleRest}}}

oodleReduce_29 = oodleSpecReduce_3  16# oodleReduction_29
oodleReduction_29 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut16 oodle_x_3 of { oodle_var_3 -> 
	oodleIn20
		 (oodle_var_3
	)}

oodleReduce_30 = oodleSpecReduce_0  16# oodleReduction_30
oodleReduction_30  =  oodleIn20
		 ([]
	)

oodleReduce_31 = oodleReduce 7# 17# oodleReduction_31
oodleReduction_31 (oodle_x_7 `OodleStk`
	oodle_x_6 `OodleStk`
	oodle_x_5 `OodleStk`
	oodle_x_4 `OodleStk`
	oodle_x_3 `OodleStk`
	oodle_x_2 `OodleStk`
	oodle_x_1 `OodleStk`
	oodleRest)
	 = case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	case oodleOut16 oodle_x_5 of { oodle_var_5 -> 
	oodleIn21
		 (LoopStatement oodle_var_3 oodle_var_5
	) `OodleStk` oodleRest}}

oodleReduce_32 = oodleReduce 4# 18# oodleReduction_32
oodleReduction_32 (oodle_x_4 `OodleStk`
	oodle_x_3 `OodleStk`
	oodle_x_2 `OodleStk`
	oodle_x_1 `OodleStk`
	oodleRest)
	 = case oodleOut23 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut24 oodle_x_3 of { oodle_var_3 -> 
	oodleIn22
		 (CallStatement (fst oodle_var_1) (snd oodle_var_1) oodle_var_3
	) `OodleStk` oodleRest}}

oodleReduce_33 = oodleSpecReduce_3  19# oodleReduction_33
oodleReduction_33 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOutTok oodle_x_3 of { (Token (TokenIdentifier oodle_var_3) _) -> 
	oodleIn23
		 ((oodle_var_1, (Id oodle_var_3))
	)}}

oodleReduce_34 = oodleSpecReduce_1  19# oodleReduction_34
oodleReduction_34 oodle_x_1
	 =  case oodleOutTok oodle_x_1 of { (Token (TokenIdentifier oodle_var_1) _) -> 
	oodleIn23
		 ((ExpressionNull, (Id oodle_var_1))
	)}

oodleReduce_35 = oodleSpecReduce_1  20# oodleReduction_35
oodleReduction_35 oodle_x_1
	 =  case oodleOut29 oodle_x_1 of { oodle_var_1 -> 
	oodleIn24
		 (oodle_var_1
	)}

oodleReduce_36 = oodleSpecReduce_0  20# oodleReduction_36
oodleReduction_36  =  oodleIn24
		 ([]
	)

oodleReduce_37 = oodleSpecReduce_0  21# oodleReduction_37
oodleReduction_37  =  oodleIn25
		 ([]
	)

oodleReduce_38 = oodleSpecReduce_2  21# oodleReduction_38
oodleReduction_38 oodle_x_2
	oodle_x_1
	 =  case oodleOut26 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut25 oodle_x_2 of { oodle_var_2 -> 
	oodleIn25
		 (oodle_var_1 : oodle_var_2
	)}}

oodleReduce_39 = oodleSpecReduce_3  22# oodleReduction_39
oodleReduction_39 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_2 of { oodle_var_2 -> 
	oodleIn26
		 (oodle_var_2
	)}

oodleReduce_40 = oodleSpecReduce_1  23# oodleReduction_40
oodleReduction_40 oodle_x_1
	 =  oodleIn27
		 (TypeInt
	)

oodleReduce_41 = oodleSpecReduce_1  23# oodleReduction_41
oodleReduction_41 oodle_x_1
	 =  oodleIn27
		 (TypeString
	)

oodleReduce_42 = oodleSpecReduce_1  23# oodleReduction_42
oodleReduction_42 oodle_x_1
	 =  oodleIn27
		 (TypeBoolean
	)

oodleReduce_43 = oodleSpecReduce_1  23# oodleReduction_43
oodleReduction_43 oodle_x_1
	 =  case oodleOutTok oodle_x_1 of { (Token (TokenIdentifier oodle_var_1) _) -> 
	oodleIn27
		 (TypeId (Id oodle_var_1)
	)}

oodleReduce_44 = oodleSpecReduce_2  23# oodleReduction_44
oodleReduction_44 oodle_x_2
	oodle_x_1
	 =  case oodleOut27 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut26 oodle_x_2 of { oodle_var_2 -> 
	oodleIn27
		 (TypeExp oodle_var_1 oodle_var_2
	)}}

oodleReduce_45 = oodleSpecReduce_1  24# oodleReduction_45
oodleReduction_45 oodle_x_1
	 =  case oodleOutTok oodle_x_1 of { (Token (TokenIdentifier oodle_var_1) _) -> 
	oodleIn28
		 (ExpressionId (Id oodle_var_1)
	)}

oodleReduce_46 = oodleSpecReduce_1  24# oodleReduction_46
oodleReduction_46 oodle_x_1
	 =  case oodleOutTok oodle_x_1 of { (Token (TokenStringLiteral oodle_var_1) _) -> 
	oodleIn28
		 (ExpressionStr oodle_var_1
	)}

oodleReduce_47 = oodleSpecReduce_1  24# oodleReduction_47
oodleReduction_47 oodle_x_1
	 =  case oodleOutTok oodle_x_1 of { (Token (TokenIntLiteral oodle_var_1) _) -> 
	oodleIn28
		 (ExpressionInt oodle_var_1
	)}

oodleReduce_48 = oodleSpecReduce_1  24# oodleReduction_48
oodleReduction_48 oodle_x_1
	 =  oodleIn28
		 (ExpressionTrue
	)

oodleReduce_49 = oodleSpecReduce_1  24# oodleReduction_49
oodleReduction_49 oodle_x_1
	 =  oodleIn28
		 (ExpressionFalse
	)

oodleReduce_50 = oodleSpecReduce_1  24# oodleReduction_50
oodleReduction_50 oodle_x_1
	 =  oodleIn28
		 (ExpressionNull
	)

oodleReduce_51 = oodleSpecReduce_1  24# oodleReduction_51
oodleReduction_51 oodle_x_1
	 =  oodleIn28
		 (ExpressionMe
	)

oodleReduce_52 = oodleSpecReduce_2  24# oodleReduction_52
oodleReduction_52 oodle_x_2
	oodle_x_1
	 =  case oodleOut27 oodle_x_2 of { oodle_var_2 -> 
	oodleIn28
		 (ExpressionType oodle_var_2
	)}

oodleReduce_53 = oodleSpecReduce_3  24# oodleReduction_53
oodleReduction_53 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_2 of { oodle_var_2 -> 
	oodleIn28
		 (oodle_var_2
	)}

oodleReduce_54 = oodleReduce 4# 24# oodleReduction_54
oodleReduction_54 (oodle_x_4 `OodleStk`
	oodle_x_3 `OodleStk`
	oodle_x_2 `OodleStk`
	oodle_x_1 `OodleStk`
	oodleRest)
	 = case oodleOut23 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut24 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionCall (fst oodle_var_1) (snd oodle_var_1) oodle_var_3
	) `OodleStk` oodleRest}}

oodleReduce_55 = oodleSpecReduce_2  24# oodleReduction_55
oodleReduction_55 oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_2 of { oodle_var_2 -> 
	oodleIn28
		 (ExpressionNot oodle_var_2
	)}

oodleReduce_56 = oodleSpecReduce_2  24# oodleReduction_56
oodleReduction_56 oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_2 of { oodle_var_2 -> 
	oodleIn28
		 (ExpressionNeg oodle_var_2
	)}

oodleReduce_57 = oodleSpecReduce_2  24# oodleReduction_57
oodleReduction_57 oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_2 of { oodle_var_2 -> 
	oodleIn28
		 (ExpressionPos oodle_var_2
	)}

oodleReduce_58 = oodleSpecReduce_3  24# oodleReduction_58
oodleReduction_58 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionMul oodle_var_1 oodle_var_3
	)}}

oodleReduce_59 = oodleSpecReduce_3  24# oodleReduction_59
oodleReduction_59 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionDiv oodle_var_1 oodle_var_3
	)}}

oodleReduce_60 = oodleSpecReduce_3  24# oodleReduction_60
oodleReduction_60 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionAdd oodle_var_1 oodle_var_3
	)}}

oodleReduce_61 = oodleSpecReduce_3  24# oodleReduction_61
oodleReduction_61 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionSub oodle_var_1 oodle_var_3
	)}}

oodleReduce_62 = oodleSpecReduce_3  24# oodleReduction_62
oodleReduction_62 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionStrCat oodle_var_1 oodle_var_3
	)}}

oodleReduce_63 = oodleSpecReduce_3  24# oodleReduction_63
oodleReduction_63 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionEq oodle_var_1 oodle_var_3
	)}}

oodleReduce_64 = oodleSpecReduce_3  24# oodleReduction_64
oodleReduction_64 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionGt oodle_var_1 oodle_var_3
	)}}

oodleReduce_65 = oodleSpecReduce_3  24# oodleReduction_65
oodleReduction_65 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionGtEq oodle_var_1 oodle_var_3
	)}}

oodleReduce_66 = oodleSpecReduce_3  24# oodleReduction_66
oodleReduction_66 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionAnd oodle_var_1 oodle_var_3
	)}}

oodleReduce_67 = oodleSpecReduce_3  24# oodleReduction_67
oodleReduction_67 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut28 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionOr oodle_var_1 oodle_var_3
	)}}

oodleReduce_68 = oodleSpecReduce_3  24# oodleReduction_68
oodleReduction_68 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOutTok oodle_x_1 of { (Token (TokenIdentifier oodle_var_1) _) -> 
	case oodleOut26 oodle_x_2 of { oodle_var_2 -> 
	case oodleOut25 oodle_x_3 of { oodle_var_3 -> 
	oodleIn28
		 (ExpressionIdArray (IdArray oodle_var_1 (oodle_var_2 : oodle_var_3))
	)}}}

oodleReduce_69 = oodleSpecReduce_1  25# oodleReduction_69
oodleReduction_69 oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	oodleIn29
		 ([oodle_var_1]
	)}

oodleReduce_70 = oodleSpecReduce_3  25# oodleReduction_70
oodleReduction_70 oodle_x_3
	oodle_x_2
	oodle_x_1
	 =  case oodleOut28 oodle_x_1 of { oodle_var_1 -> 
	case oodleOut29 oodle_x_3 of { oodle_var_3 -> 
	oodleIn29
		 (oodle_var_1 : oodle_var_3
	)}}

oodleReduce_71 = oodleSpecReduce_1  26# oodleReduction_71
oodleReduction_71 oodle_x_1
	 =  oodleIn30
		 (
	)

oodleReduce_72 = oodleSpecReduce_2  26# oodleReduction_72
oodleReduction_72 oodle_x_2
	oodle_x_1
	 =  oodleIn30
		 (
	)

oodleNewToken action sts stk [] =
	oodleDoAction 44# notOodleAtAll action sts stk []

oodleNewToken action sts stk (tk:tks) =
	let cont i = oodleDoAction i tk action sts stk tks in
	case tk of {
	Token TokenNewline _ -> cont 1#;
	Token (TokenIntLiteral oodle_dollar_dollar) _ -> cont 2#;
	Token (TokenStringLiteral oodle_dollar_dollar) _ -> cont 3#;
	Token (TokenIdentifier oodle_dollar_dollar) _ -> cont 4#;
	Token TokenBoolean _ -> cont 5#;
	Token TokenBegin _ -> cont 6#;
	Token TokenClass _ -> cont 7#;
	Token TokenElse _ -> cont 8#;
	Token TokenEnd _ -> cont 9#;
	Token TokenFalse _ -> cont 10#;
	Token TokenFrom _ -> cont 11#;
	Token TokenIf _ -> cont 12#;
	Token TokenInherits _ -> cont 13#;
	Token TokenInt _ -> cont 14#;
	Token TokenIs _ -> cont 15#;
	Token TokenLoop _ -> cont 16#;
	Token TokenMe _ -> cont 17#;
	Token TokenNew _ -> cont 18#;
	Token TokenNot _ -> cont 19#;
	Token TokenNull _ -> cont 20#;
	Token TokenString _ -> cont 21#;
	Token TokenThen _ -> cont 22#;
	Token TokenTrue _ -> cont 23#;
	Token TokenWhile _ -> cont 24#;
	Token TokenAnd _ -> cont 25#;
	Token TokenOr _ -> cont 26#;
	Token TokenStringConcat _ -> cont 27#;
	Token TokenPlus _ -> cont 28#;
	Token TokenMinus _ -> cont 29#;
	Token TokenTimes _ -> cont 30#;
	Token TokenDiv _ -> cont 31#;
	Token TokenGT _ -> cont 32#;
	Token TokenGTEq _ -> cont 33#;
	Token TokenEq _ -> cont 34#;
	Token TokenAssign _ -> cont 35#;
	Token TokenOP _ -> cont 36#;
	Token TokenCP _ -> cont 37#;
	Token TokenOB _ -> cont 38#;
	Token TokenCB _ -> cont 39#;
	Token TokenComma _ -> cont 40#;
	Token TokenSemicolon _ -> cont 41#;
	Token TokenColon _ -> cont 42#;
	Token TokenPeriod _ -> cont 43#;
	_ -> oodleError' (tk:tks)
	}

oodleError_ 44# tk tks = oodleError' tks
oodleError_ _ tk tks = oodleError' (tk:tks)

oodleThen :: () => E a -> (a -> E b) -> E b
oodleThen = (thenE)
oodleReturn :: () => a -> E a
oodleReturn = (returnE)
oodleThen1 m k tks = (thenE) m (\a -> k a tks)
oodleReturn1 :: () => a -> b -> E a
oodleReturn1 = \a tks -> (returnE) a
oodleError' :: () => [(Token)] -> E a
oodleError' = parseError

parser tks = oodleSomeParser where
  oodleSomeParser = oodleThen (oodleParse 0# tks) (\x -> oodleReturn (oodleOut4 x))

oodleSeq = oodleDontSeq


data E a = Ok a | Failed String
  deriving (Show)

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
  case m of
    Ok a -> k a
    Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
  case m of
    Ok a -> Ok a
    Failed e -> k e


parseError tokenStream = failE $ "Parse error at: " ++ (printToken (head tokenStream))

data Start
      = Start [Class]
  deriving Show

data Class
      = Class Id Id [Var] [Method] Id
  deriving Show

data Var
      = Var Id Type Expression
  deriving Show

data Method
      -- Id = Method name
      -- Type = return type
      -- [Argument] = arguments
      -- [Var] = variable declarations
      -- [Statement] = statments
      -- Id = ending method name (should match the first)
      = Method Id Type [Argument] [Var] [Statement] Id
  deriving Show

data Argument = Argument Id Type
  deriving Show

data Statement
      = AssignStatement Id Expression
      | IfStatement Expression [Statement] [Statement]
      | LoopStatement Expression [Statement]
      | CallStatement Expression Id [Expression]
  deriving Show

data Id
      = Id String
      | IdArray String [Expression]
  deriving Show

data Type
      = TypeInt
      | TypeNull
      | TypeString
      | TypeBoolean
      | TypeId Id
      | TypeExp Type Expression
  deriving Show

data Expression
      = ExpressionInt Int
      | ExpressionId Id
      | ExpressionStr String
      | ExpressionTrue
      | ExpressionFalse
      | ExpressionMe
      | ExpressionType Type
      | ExpressionCall Expression Id [Expression]
      | ExpressionIdArray Id
      | ExpressionNot Expression
      | ExpressionNeg Expression
      | ExpressionPos Expression
      | ExpressionMul Expression Expression
      | ExpressionDiv Expression Expression
      | ExpressionAdd Expression Expression
      | ExpressionSub Expression Expression
      | ExpressionStrCat Expression Expression
      | ExpressionEq Expression Expression
      | ExpressionGt Expression Expression
      | ExpressionGtEq Expression Expression
      | ExpressionAnd Expression Expression
      | ExpressionOr Expression Expression
      | ExpressionNull
  deriving Show
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Oodle_GHC_Exts.tagToEnum# (n Oodle_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Oodle_GHC_Exts.tagToEnum# (n Oodle_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Oodle_GHC_Exts.tagToEnum# (n Oodle_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Oodle_GHC_Exts.<# m)
#define GTE(n,m) (n Oodle_GHC_Exts.>=# m)
#define EQ(n,m) (n Oodle_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Oodle_IntList = OodleCons Oodle_GHC_Exts.Int# Oodle_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `OodleStk`
data OodleStk a = OodleStk a (OodleStk a)

-----------------------------------------------------------------------------
-- starting the parse

oodleParse start_state = oodleNewToken start_state notOodleAtAll notOodleAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
oodleAccept 0# tk st sts (_ `OodleStk` ans `OodleStk` _) =
        oodleReturn1 ans
oodleAccept j tk st sts (OodleStk ans _) = 
        (oodleTcHack j (oodleTcHack st)) (oodleReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



oodleDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     oodleFail i tk st
                -1#          -> {- nothing -}
                                     oodleAccept i tk st
                n | LT(n,(0# :: Oodle_GHC_Exts.Int#)) -> {- nothing -}

                                                   (oodleReduceArr Oodle_Data_Array.! rule) i tk st
                                                   where rule = (Oodle_GHC_Exts.I# ((Oodle_GHC_Exts.negateInt# ((n Oodle_GHC_Exts.+# (1# :: Oodle_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     oodleShift new_state i tk st
                                     where new_state = (n Oodle_GHC_Exts.-# (1# :: Oodle_GHC_Exts.Int#))
   where off    = indexShortOffAddr oodleActOffsets st
         off_i  = (off Oodle_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Oodle_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr oodleCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr oodleTable off_i
          | otherwise = indexShortOffAddr oodleDefActions st


indexShortOffAddr (OodleA# arr) off =
        Oodle_GHC_Exts.narrow16Int# i
  where
        i = Oodle_GHC_Exts.word2Int# (Oodle_GHC_Exts.or# (Oodle_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Oodle_GHC_Exts.int2Word# (Oodle_GHC_Exts.ord# (Oodle_GHC_Exts.indexCharOffAddr# arr (off' Oodle_GHC_Exts.+# 1#)))
        low  = Oodle_GHC_Exts.int2Word# (Oodle_GHC_Exts.ord# (Oodle_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Oodle_GHC_Exts.*# 2#





data OodleAddr = OodleA# Oodle_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- OodleState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

oodleShift new_state 0# tk st sts stk@(x `OodleStk` _) =
     let i = (case Oodle_GHC_Exts.unsafeCoerce# x of { (Oodle_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     oodleDoAction i tk new_state (OodleCons (st) (sts)) (stk)

oodleShift new_state i tk st sts stk =
     oodleNewToken new_state (OodleCons (st) (sts)) ((oodleInTok (tk))`OodleStk`stk)

-- oodleReduce is specialised for the common cases.

oodleSpecReduce_0 i fn 0# tk st sts stk
     = oodleFail 0# tk st sts stk
oodleSpecReduce_0 nt fn j tk st@((action)) sts stk
     = oodleGoto nt j tk st (OodleCons (st) (sts)) (fn `OodleStk` stk)

oodleSpecReduce_1 i fn 0# tk st sts stk
     = oodleFail 0# tk st sts stk
oodleSpecReduce_1 nt fn j tk _ sts@((OodleCons (st@(action)) (_))) (v1`OodleStk`stk')
     = let r = fn v1 in
       oodleSeq r (oodleGoto nt j tk st sts (r `OodleStk` stk'))

oodleSpecReduce_2 i fn 0# tk st sts stk
     = oodleFail 0# tk st sts stk
oodleSpecReduce_2 nt fn j tk _ (OodleCons (_) (sts@((OodleCons (st@(action)) (_))))) (v1`OodleStk`v2`OodleStk`stk')
     = let r = fn v1 v2 in
       oodleSeq r (oodleGoto nt j tk st sts (r `OodleStk` stk'))

oodleSpecReduce_3 i fn 0# tk st sts stk
     = oodleFail 0# tk st sts stk
oodleSpecReduce_3 nt fn j tk _ (OodleCons (_) ((OodleCons (_) (sts@((OodleCons (st@(action)) (_))))))) (v1`OodleStk`v2`OodleStk`v3`OodleStk`stk')
     = let r = fn v1 v2 v3 in
       oodleSeq r (oodleGoto nt j tk st sts (r `OodleStk` stk'))

oodleReduce k i fn 0# tk st sts stk
     = oodleFail 0# tk st sts stk
oodleReduce k nt fn j tk st sts stk
     = case oodleDrop (k Oodle_GHC_Exts.-# (1# :: Oodle_GHC_Exts.Int#)) sts of
         sts1@((OodleCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                oodleDoSeq r (oodleGoto nt j tk st1 sts1 r)

oodleMonadReduce k nt fn 0# tk st sts stk
     = oodleFail 0# tk st sts stk
oodleMonadReduce k nt fn j tk st sts stk =
      case oodleDrop k (OodleCons (st) (sts)) of
        sts1@((OodleCons (st1@(action)) (_))) ->
          let drop_stk = oodleDropStk k stk in
          oodleThen1 (fn stk tk) (\r -> oodleGoto nt j tk st1 sts1 (r `OodleStk` drop_stk))

oodleMonad2Reduce k nt fn 0# tk st sts stk
     = oodleFail 0# tk st sts stk
oodleMonad2Reduce k nt fn j tk st sts stk =
      case oodleDrop k (OodleCons (st) (sts)) of
        sts1@((OodleCons (st1@(action)) (_))) ->
         let drop_stk = oodleDropStk k stk

             off = indexShortOffAddr oodleGotoOffsets st1
             off_i = (off Oodle_GHC_Exts.+# nt)
             new_state = indexShortOffAddr oodleTable off_i



          in
          oodleThen1 (fn stk tk) (\r -> oodleNewToken new_state sts1 (r `OodleStk` drop_stk))

oodleDrop 0# l = l
oodleDrop n (OodleCons (_) (t)) = oodleDrop (n Oodle_GHC_Exts.-# (1# :: Oodle_GHC_Exts.Int#)) t

oodleDropStk 0# l = l
oodleDropStk n (x `OodleStk` xs) = oodleDropStk (n Oodle_GHC_Exts.-# (1#::Oodle_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


oodleGoto nt j tk st = 
   {- nothing -}
   oodleDoAction j tk new_state
   where off = indexShortOffAddr oodleGotoOffsets st
         off_i = (off Oodle_GHC_Exts.+# nt)
         new_state = indexShortOffAddr oodleTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
oodleFail 0# tk old_st _ stk@(x `OodleStk` _) =
     let i = (case Oodle_GHC_Exts.unsafeCoerce# x of { (Oodle_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        oodleError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
oodleFail  0# tk old_st (OodleCons ((action)) (sts)) 
                                                (saved_tok `OodleStk` _ `OodleStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        oodleDoAction 0# tk action sts ((saved_tok`OodleStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
oodleFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        oodleDoAction 0# tk action sts ( (Oodle_GHC_Exts.unsafeCoerce# (Oodle_GHC_Exts.I# (i))) `OodleStk` stk)

-- Internal oodle errors:

notOodleAtAll :: a
notOodleAtAll = error "Internal Oodle error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


oodleTcHack :: Oodle_GHC_Exts.Int# -> a -> a
oodleTcHack x y = y
{-# INLINE oodleTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Oodle emits 
--      oodleSeq = oodleDoSeq
-- otherwise it emits
--      oodleSeq = oodleDontSeq

oodleDoSeq, oodleDontSeq :: a -> b -> b
oodleDoSeq   a b = a `seq` b
oodleDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline oodleGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE oodleDoAction #-}
{-# NOINLINE oodleTable #-}
{-# NOINLINE oodleCheck #-}
{-# NOINLINE oodleActOffsets #-}
{-# NOINLINE oodleGotoOffsets #-}
{-# NOINLINE oodleDefActions #-}

{-# NOINLINE oodleShift #-}
{-# NOINLINE oodleSpecReduce_0 #-}
{-# NOINLINE oodleSpecReduce_1 #-}
{-# NOINLINE oodleSpecReduce_2 #-}
{-# NOINLINE oodleSpecReduce_3 #-}
{-# NOINLINE oodleReduce #-}
{-# NOINLINE oodleMonadReduce #-}
{-# NOINLINE oodleGoto #-}
{-# NOINLINE oodleFail #-}

-- end of Oodle Template.
