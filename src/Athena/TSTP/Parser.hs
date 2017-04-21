{-# OPTIONS_GHC -w #-}
-- | Athena.TSTP.Parser module
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# OPTIONS -fno-warn-incomplete-patterns       #-}
{-# OPTIONS -fno-warn-incomplete-uni-patterns   #-}
{-# OPTIONS -fno-warn-missing-signatures        #-}
{-# OPTIONS -fno-warn-monomorphism-restriction  #-}
{-# OPTIONS -fno-warn-name-shadowing            #-}
{-# OPTIONS -fno-warn-unused-matches            #-}

module Athena.TSTP.Parser where

------------------------------------------------------------------------------

import Athena.TSTP.Base
import Athena.TSTP.Lexer

import Control.Monad
import Control.Monad.Identity

import Data.Char
import Data.Data
import Data.List as L
import Data.Ratio
import Data.Set as S

import Data.TSTP
  ( AtomicWord (..)
  , BinOp (..)
  , F (..)
  , Formula (..)
  , GData (..)
  , GTerm (..)
  , InfixPred (..)
  , Info (..)
  , IntroType (..)
  , Parent (..)
  , Quant (..)
  , Role (..)
  , Rule (..)
  , Source (..)
  , Status (..)
  , Term (..)
  , Theory (..)
  , V (..)
  )

import System.IO
import System.IO.Unsafe

------------------------------------------------------------------------------
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([F])
	| HappyAbsSyn5 (F)
	| HappyAbsSyn9 (Source)
	| HappyAbsSyn10 (Role)
	| HappyAbsSyn11 (Formula)
	| HappyAbsSyn16 ([Formula])
	| HappyAbsSyn21 ([V])
	| HappyAbsSyn28 (Quant)
	| HappyAbsSyn29 (BinOp)
	| HappyAbsSyn35 (InfixPred)
	| HappyAbsSyn39 (Term)
	| HappyAbsSyn42 (AtomicWord)
	| HappyAbsSyn48 (String)
	| HappyAbsSyn53 (V)
	| HappyAbsSyn54 ([Term])
	| HappyAbsSyn58 (Rule)
	| HappyAbsSyn59 ([Parent])
	| HappyAbsSyn60 (Parent)
	| HappyAbsSyn61 ([GTerm])
	| HappyAbsSyn63 (IntroType)
	| HappyAbsSyn66 (Maybe String)
	| HappyAbsSyn68 (Theory)
	| HappyAbsSyn71 ([Info])
	| HappyAbsSyn74 (Info)
	| HappyAbsSyn81 (Status)
	| HappyAbsSyn85 ([AtomicWord])
	| HappyAbsSyn86 (GTerm)
	| HappyAbsSyn87 (GData)
	| HappyAbsSyn95 (Rational)
	| HappyAbsSyn96 (Integer)
	| HappyAbsSyn100 (Token)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316,
 action_317,
 action_318,
 action_319,
 action_320,
 action_321,
 action_322,
 action_323,
 action_324,
 action_325,
 action_326,
 action_327,
 action_328,
 action_329,
 action_330,
 action_331,
 action_332,
 action_333,
 action_334,
 action_335,
 action_336,
 action_337,
 action_338,
 action_339,
 action_340 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164,
 happyReduce_165,
 happyReduce_166,
 happyReduce_167,
 happyReduce_168,
 happyReduce_169,
 happyReduce_170,
 happyReduce_171,
 happyReduce_172,
 happyReduce_173,
 happyReduce_174,
 happyReduce_175,
 happyReduce_176,
 happyReduce_177,
 happyReduce_178,
 happyReduce_179,
 happyReduce_180,
 happyReduce_181,
 happyReduce_182,
 happyReduce_183,
 happyReduce_184,
 happyReduce_185,
 happyReduce_186,
 happyReduce_187,
 happyReduce_188,
 happyReduce_189,
 happyReduce_190,
 happyReduce_191,
 happyReduce_192,
 happyReduce_193,
 happyReduce_194,
 happyReduce_195,
 happyReduce_196,
 happyReduce_197,
 happyReduce_198,
 happyReduce_199,
 happyReduce_200,
 happyReduce_201,
 happyReduce_202,
 happyReduce_203,
 happyReduce_204,
 happyReduce_205,
 happyReduce_206,
 happyReduce_207,
 happyReduce_208,
 happyReduce_209,
 happyReduce_210,
 happyReduce_211,
 happyReduce_212,
 happyReduce_213,
 happyReduce_214,
 happyReduce_215 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (167) = happyShift action_9
action_0 (172) = happyShift action_10
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (7) = happyGoto action_5
action_0 (8) = happyGoto action_6
action_0 (120) = happyGoto action_7
action_0 (121) = happyGoto action_8
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (193) = happyAccept
action_2 _ = happyFail

action_3 (167) = happyShift action_9
action_3 (172) = happyShift action_10
action_3 (4) = happyGoto action_14
action_3 (5) = happyGoto action_3
action_3 (6) = happyGoto action_4
action_3 (7) = happyGoto action_5
action_3 (8) = happyGoto action_6
action_3 (120) = happyGoto action_7
action_3 (121) = happyGoto action_8
action_3 _ = happyReduce_1

action_4 _ = happyReduce_3

action_5 _ = happyReduce_5

action_6 _ = happyReduce_4

action_7 (149) = happyShift action_12
action_7 (100) = happyGoto action_13
action_7 _ = happyFail

action_8 (149) = happyShift action_12
action_8 (100) = happyGoto action_11
action_8 _ = happyFail

action_9 _ = happyReduce_192

action_10 _ = happyReduce_191

action_11 (167) = happyShift action_9
action_11 (172) = happyShift action_10
action_11 (173) = happyShift action_25
action_11 (180) = happyShift action_26
action_11 (186) = happyShift action_27
action_11 (189) = happyShift action_28
action_11 (191) = happyShift action_29
action_11 (91) = happyGoto action_30
action_11 (92) = happyGoto action_16
action_11 (99) = happyGoto action_17
action_11 (120) = happyGoto action_18
action_11 (121) = happyGoto action_19
action_11 (122) = happyGoto action_20
action_11 (123) = happyGoto action_21
action_11 (124) = happyGoto action_22
action_11 (129) = happyGoto action_23
action_11 (131) = happyGoto action_24
action_11 _ = happyFail

action_12 _ = happyReduce_171

action_13 (167) = happyShift action_9
action_13 (172) = happyShift action_10
action_13 (173) = happyShift action_25
action_13 (180) = happyShift action_26
action_13 (186) = happyShift action_27
action_13 (189) = happyShift action_28
action_13 (191) = happyShift action_29
action_13 (91) = happyGoto action_15
action_13 (92) = happyGoto action_16
action_13 (99) = happyGoto action_17
action_13 (120) = happyGoto action_18
action_13 (121) = happyGoto action_19
action_13 (122) = happyGoto action_20
action_13 (123) = happyGoto action_21
action_13 (124) = happyGoto action_22
action_13 (129) = happyGoto action_23
action_13 (131) = happyGoto action_24
action_13 _ = happyFail

action_14 _ = happyReduce_2

action_15 (146) = happyShift action_32
action_15 (104) = happyGoto action_33
action_15 _ = happyFail

action_16 _ = happyReduce_153

action_17 _ = happyReduce_155

action_18 _ = happyReduce_168

action_19 _ = happyReduce_166

action_20 _ = happyReduce_167

action_21 _ = happyReduce_169

action_22 _ = happyReduce_156

action_23 _ = happyReduce_170

action_24 _ = happyReduce_154

action_25 _ = happyReduce_194

action_26 _ = happyReduce_193

action_27 _ = happyReduce_200

action_28 _ = happyReduce_195

action_29 _ = happyReduce_202

action_30 (146) = happyShift action_32
action_30 (104) = happyGoto action_31
action_30 _ = happyFail

action_31 (167) = happyShift action_9
action_31 (172) = happyShift action_10
action_31 (173) = happyShift action_25
action_31 (180) = happyShift action_26
action_31 (186) = happyShift action_27
action_31 (10) = happyGoto action_36
action_31 (99) = happyGoto action_35
action_31 (120) = happyGoto action_18
action_31 (121) = happyGoto action_19
action_31 (122) = happyGoto action_20
action_31 (123) = happyGoto action_21
action_31 (129) = happyGoto action_23
action_31 _ = happyFail

action_32 _ = happyReduce_175

action_33 (167) = happyShift action_9
action_33 (172) = happyShift action_10
action_33 (173) = happyShift action_25
action_33 (180) = happyShift action_26
action_33 (186) = happyShift action_27
action_33 (10) = happyGoto action_34
action_33 (99) = happyGoto action_35
action_33 (120) = happyGoto action_18
action_33 (121) = happyGoto action_19
action_33 (122) = happyGoto action_20
action_33 (123) = happyGoto action_21
action_33 (129) = happyGoto action_23
action_33 _ = happyFail

action_34 (146) = happyShift action_32
action_34 (104) = happyGoto action_38
action_34 _ = happyFail

action_35 _ = happyReduce_10

action_36 (146) = happyShift action_32
action_36 (104) = happyGoto action_37
action_36 _ = happyFail

action_37 (149) = happyShift action_12
action_37 (163) = happyShift action_90
action_37 (167) = happyShift action_9
action_37 (172) = happyShift action_10
action_37 (173) = happyShift action_25
action_37 (180) = happyShift action_26
action_37 (183) = happyShift action_91
action_37 (184) = happyShift action_92
action_37 (185) = happyShift action_93
action_37 (186) = happyShift action_27
action_37 (187) = happyShift action_94
action_37 (188) = happyShift action_95
action_37 (189) = happyShift action_28
action_37 (191) = happyShift action_29
action_37 (192) = happyShift action_96
action_37 (23) = happyGoto action_97
action_37 (24) = happyGoto action_98
action_37 (26) = happyGoto action_99
action_37 (27) = happyGoto action_100
action_37 (30) = happyGoto action_101
action_37 (31) = happyGoto action_51
action_37 (32) = happyGoto action_52
action_37 (33) = happyGoto action_53
action_37 (34) = happyGoto action_54
action_37 (38) = happyGoto action_55
action_37 (39) = happyGoto action_56
action_37 (40) = happyGoto action_57
action_37 (41) = happyGoto action_58
action_37 (42) = happyGoto action_59
action_37 (43) = happyGoto action_60
action_37 (44) = happyGoto action_61
action_37 (45) = happyGoto action_62
action_37 (46) = happyGoto action_63
action_37 (47) = happyGoto action_64
action_37 (48) = happyGoto action_65
action_37 (49) = happyGoto action_66
action_37 (50) = happyGoto action_67
action_37 (51) = happyGoto action_68
action_37 (52) = happyGoto action_69
action_37 (53) = happyGoto action_70
action_37 (92) = happyGoto action_71
action_37 (93) = happyGoto action_72
action_37 (94) = happyGoto action_73
action_37 (95) = happyGoto action_74
action_37 (96) = happyGoto action_75
action_37 (97) = happyGoto action_76
action_37 (99) = happyGoto action_17
action_37 (100) = happyGoto action_102
action_37 (119) = happyGoto action_103
action_37 (120) = happyGoto action_18
action_37 (121) = happyGoto action_19
action_37 (122) = happyGoto action_20
action_37 (123) = happyGoto action_21
action_37 (124) = happyGoto action_22
action_37 (125) = happyGoto action_81
action_37 (126) = happyGoto action_82
action_37 (127) = happyGoto action_83
action_37 (128) = happyGoto action_84
action_37 (129) = happyGoto action_23
action_37 (130) = happyGoto action_85
action_37 (131) = happyGoto action_86
action_37 (132) = happyGoto action_87
action_37 _ = happyFail

action_38 (149) = happyShift action_12
action_38 (161) = happyShift action_88
action_38 (162) = happyShift action_89
action_38 (163) = happyShift action_90
action_38 (167) = happyShift action_9
action_38 (172) = happyShift action_10
action_38 (173) = happyShift action_25
action_38 (180) = happyShift action_26
action_38 (183) = happyShift action_91
action_38 (184) = happyShift action_92
action_38 (185) = happyShift action_93
action_38 (186) = happyShift action_27
action_38 (187) = happyShift action_94
action_38 (188) = happyShift action_95
action_38 (189) = happyShift action_28
action_38 (191) = happyShift action_29
action_38 (192) = happyShift action_96
action_38 (11) = happyGoto action_39
action_38 (12) = happyGoto action_40
action_38 (13) = happyGoto action_41
action_38 (14) = happyGoto action_42
action_38 (15) = happyGoto action_43
action_38 (17) = happyGoto action_44
action_38 (19) = happyGoto action_45
action_38 (20) = happyGoto action_46
action_38 (22) = happyGoto action_47
action_38 (27) = happyGoto action_48
action_38 (28) = happyGoto action_49
action_38 (30) = happyGoto action_50
action_38 (31) = happyGoto action_51
action_38 (32) = happyGoto action_52
action_38 (33) = happyGoto action_53
action_38 (34) = happyGoto action_54
action_38 (38) = happyGoto action_55
action_38 (39) = happyGoto action_56
action_38 (40) = happyGoto action_57
action_38 (41) = happyGoto action_58
action_38 (42) = happyGoto action_59
action_38 (43) = happyGoto action_60
action_38 (44) = happyGoto action_61
action_38 (45) = happyGoto action_62
action_38 (46) = happyGoto action_63
action_38 (47) = happyGoto action_64
action_38 (48) = happyGoto action_65
action_38 (49) = happyGoto action_66
action_38 (50) = happyGoto action_67
action_38 (51) = happyGoto action_68
action_38 (52) = happyGoto action_69
action_38 (53) = happyGoto action_70
action_38 (92) = happyGoto action_71
action_38 (93) = happyGoto action_72
action_38 (94) = happyGoto action_73
action_38 (95) = happyGoto action_74
action_38 (96) = happyGoto action_75
action_38 (97) = happyGoto action_76
action_38 (99) = happyGoto action_17
action_38 (100) = happyGoto action_77
action_38 (115) = happyGoto action_78
action_38 (116) = happyGoto action_79
action_38 (119) = happyGoto action_80
action_38 (120) = happyGoto action_18
action_38 (121) = happyGoto action_19
action_38 (122) = happyGoto action_20
action_38 (123) = happyGoto action_21
action_38 (124) = happyGoto action_22
action_38 (125) = happyGoto action_81
action_38 (126) = happyGoto action_82
action_38 (127) = happyGoto action_83
action_38 (128) = happyGoto action_84
action_38 (129) = happyGoto action_23
action_38 (130) = happyGoto action_85
action_38 (131) = happyGoto action_86
action_38 (132) = happyGoto action_87
action_38 _ = happyFail

action_39 (146) = happyShift action_32
action_39 (9) = happyGoto action_143
action_39 (104) = happyGoto action_111
action_39 _ = happyReduce_9

action_40 _ = happyReduce_11

action_41 _ = happyReduce_13

action_42 _ = happyReduce_14

action_43 _ = happyReduce_17

action_44 _ = happyReduce_16

action_45 (153) = happyShift action_136
action_45 (154) = happyShift action_137
action_45 (155) = happyShift action_138
action_45 (156) = happyShift action_139
action_45 (158) = happyShift action_140
action_45 (159) = happyShift action_141
action_45 (160) = happyShift action_142
action_45 (164) = happyShift action_109
action_45 (29) = happyGoto action_127
action_45 (107) = happyGoto action_128
action_45 (108) = happyGoto action_129
action_45 (109) = happyGoto action_130
action_45 (110) = happyGoto action_131
action_45 (111) = happyGoto action_132
action_45 (112) = happyGoto action_133
action_45 (117) = happyGoto action_134
action_45 (118) = happyGoto action_135
action_45 _ = happyReduce_12

action_46 _ = happyReduce_26

action_47 _ = happyReduce_27

action_48 _ = happyReduce_31

action_49 (148) = happyShift action_126
action_49 (102) = happyGoto action_125
action_49 _ = happyFail

action_50 _ = happyReduce_24

action_51 _ = happyReduce_51

action_52 _ = happyReduce_50

action_53 _ = happyReduce_55

action_54 _ = happyReduce_54

action_55 _ = happyReduce_52

action_56 (152) = happyShift action_123
action_56 (157) = happyShift action_124
action_56 (35) = happyGoto action_118
action_56 (36) = happyGoto action_119
action_56 (37) = happyGoto action_120
action_56 (113) = happyGoto action_121
action_56 (114) = happyGoto action_122
action_56 _ = happyFail

action_57 _ = happyReduce_62

action_58 (152) = happyReduce_65
action_58 (157) = happyReduce_65
action_58 _ = happyReduce_53

action_59 _ = happyReduce_67

action_60 (149) = happyShift action_12
action_60 (100) = happyGoto action_117
action_60 _ = happyReduce_69

action_61 _ = happyReduce_64

action_62 _ = happyReduce_71

action_63 _ = happyReduce_72

action_64 (152) = happyReduce_75
action_64 (157) = happyReduce_75
action_64 _ = happyReduce_56

action_65 _ = happyReduce_76

action_66 (149) = happyShift action_12
action_66 (100) = happyGoto action_116
action_66 _ = happyReduce_78

action_67 (152) = happyReduce_66
action_67 (157) = happyReduce_66
action_67 _ = happyReduce_61

action_68 _ = happyReduce_80

action_69 (149) = happyShift action_12
action_69 (100) = happyGoto action_115
action_69 _ = happyReduce_82

action_70 _ = happyReduce_63

action_71 _ = happyReduce_70

action_72 _ = happyReduce_79

action_73 _ = happyReduce_83

action_74 _ = happyReduce_74

action_75 (190) = happyShift action_114
action_75 _ = happyReduce_159

action_76 _ = happyReduce_160

action_77 (149) = happyShift action_12
action_77 (161) = happyShift action_88
action_77 (162) = happyShift action_89
action_77 (163) = happyShift action_90
action_77 (167) = happyShift action_9
action_77 (172) = happyShift action_10
action_77 (173) = happyShift action_25
action_77 (180) = happyShift action_26
action_77 (183) = happyShift action_91
action_77 (184) = happyShift action_92
action_77 (185) = happyShift action_93
action_77 (186) = happyShift action_27
action_77 (187) = happyShift action_94
action_77 (188) = happyShift action_95
action_77 (189) = happyShift action_28
action_77 (191) = happyShift action_29
action_77 (192) = happyShift action_96
action_77 (11) = happyGoto action_113
action_77 (12) = happyGoto action_40
action_77 (13) = happyGoto action_41
action_77 (14) = happyGoto action_42
action_77 (15) = happyGoto action_43
action_77 (17) = happyGoto action_44
action_77 (19) = happyGoto action_45
action_77 (20) = happyGoto action_46
action_77 (22) = happyGoto action_47
action_77 (27) = happyGoto action_48
action_77 (28) = happyGoto action_49
action_77 (30) = happyGoto action_50
action_77 (31) = happyGoto action_51
action_77 (32) = happyGoto action_52
action_77 (33) = happyGoto action_53
action_77 (34) = happyGoto action_54
action_77 (38) = happyGoto action_55
action_77 (39) = happyGoto action_56
action_77 (40) = happyGoto action_57
action_77 (41) = happyGoto action_58
action_77 (42) = happyGoto action_59
action_77 (43) = happyGoto action_60
action_77 (44) = happyGoto action_61
action_77 (45) = happyGoto action_62
action_77 (46) = happyGoto action_63
action_77 (47) = happyGoto action_64
action_77 (48) = happyGoto action_65
action_77 (49) = happyGoto action_66
action_77 (50) = happyGoto action_67
action_77 (51) = happyGoto action_68
action_77 (52) = happyGoto action_69
action_77 (53) = happyGoto action_70
action_77 (92) = happyGoto action_71
action_77 (93) = happyGoto action_72
action_77 (94) = happyGoto action_73
action_77 (95) = happyGoto action_74
action_77 (96) = happyGoto action_75
action_77 (97) = happyGoto action_76
action_77 (99) = happyGoto action_17
action_77 (100) = happyGoto action_77
action_77 (115) = happyGoto action_78
action_77 (116) = happyGoto action_79
action_77 (119) = happyGoto action_80
action_77 (120) = happyGoto action_18
action_77 (121) = happyGoto action_19
action_77 (122) = happyGoto action_20
action_77 (123) = happyGoto action_21
action_77 (124) = happyGoto action_22
action_77 (125) = happyGoto action_81
action_77 (126) = happyGoto action_82
action_77 (127) = happyGoto action_83
action_77 (128) = happyGoto action_84
action_77 (129) = happyGoto action_23
action_77 (130) = happyGoto action_85
action_77 (131) = happyGoto action_86
action_77 (132) = happyGoto action_87
action_77 _ = happyFail

action_78 _ = happyReduce_42

action_79 _ = happyReduce_43

action_80 (149) = happyShift action_12
action_80 (161) = happyShift action_88
action_80 (162) = happyShift action_89
action_80 (163) = happyShift action_90
action_80 (167) = happyShift action_9
action_80 (172) = happyShift action_10
action_80 (173) = happyShift action_25
action_80 (180) = happyShift action_26
action_80 (183) = happyShift action_91
action_80 (184) = happyShift action_92
action_80 (185) = happyShift action_93
action_80 (186) = happyShift action_27
action_80 (187) = happyShift action_94
action_80 (188) = happyShift action_95
action_80 (189) = happyShift action_28
action_80 (191) = happyShift action_29
action_80 (192) = happyShift action_96
action_80 (19) = happyGoto action_112
action_80 (20) = happyGoto action_46
action_80 (22) = happyGoto action_47
action_80 (27) = happyGoto action_48
action_80 (28) = happyGoto action_49
action_80 (30) = happyGoto action_50
action_80 (31) = happyGoto action_51
action_80 (32) = happyGoto action_52
action_80 (33) = happyGoto action_53
action_80 (34) = happyGoto action_54
action_80 (38) = happyGoto action_55
action_80 (39) = happyGoto action_56
action_80 (40) = happyGoto action_57
action_80 (41) = happyGoto action_58
action_80 (42) = happyGoto action_59
action_80 (43) = happyGoto action_60
action_80 (44) = happyGoto action_61
action_80 (45) = happyGoto action_62
action_80 (46) = happyGoto action_63
action_80 (47) = happyGoto action_64
action_80 (48) = happyGoto action_65
action_80 (49) = happyGoto action_66
action_80 (50) = happyGoto action_67
action_80 (51) = happyGoto action_68
action_80 (52) = happyGoto action_69
action_80 (53) = happyGoto action_70
action_80 (92) = happyGoto action_71
action_80 (93) = happyGoto action_72
action_80 (94) = happyGoto action_73
action_80 (95) = happyGoto action_74
action_80 (96) = happyGoto action_75
action_80 (97) = happyGoto action_76
action_80 (99) = happyGoto action_17
action_80 (100) = happyGoto action_77
action_80 (115) = happyGoto action_78
action_80 (116) = happyGoto action_79
action_80 (119) = happyGoto action_80
action_80 (120) = happyGoto action_18
action_80 (121) = happyGoto action_19
action_80 (122) = happyGoto action_20
action_80 (123) = happyGoto action_21
action_80 (124) = happyGoto action_22
action_80 (125) = happyGoto action_81
action_80 (126) = happyGoto action_82
action_80 (127) = happyGoto action_83
action_80 (128) = happyGoto action_84
action_80 (129) = happyGoto action_23
action_80 (130) = happyGoto action_85
action_80 (131) = happyGoto action_86
action_80 (132) = happyGoto action_87
action_80 _ = happyFail

action_81 _ = happyReduce_73

action_82 _ = happyReduce_157

action_83 _ = happyReduce_158

action_84 _ = happyReduce_84

action_85 _ = happyReduce_162

action_86 _ = happyReduce_163

action_87 _ = happyReduce_161

action_88 _ = happyReduce_186

action_89 _ = happyReduce_187

action_90 _ = happyReduce_190

action_91 _ = happyReduce_196

action_92 _ = happyReduce_198

action_93 _ = happyReduce_197

action_94 _ = happyReduce_203

action_95 _ = happyReduce_201

action_96 _ = happyReduce_199

action_97 (146) = happyShift action_32
action_97 (9) = happyGoto action_110
action_97 (104) = happyGoto action_111
action_97 _ = happyReduce_9

action_98 _ = happyReduce_33

action_99 (164) = happyShift action_109
action_99 (25) = happyGoto action_107
action_99 (118) = happyGoto action_108
action_99 _ = happyReduce_36

action_100 _ = happyReduce_40

action_101 _ = happyReduce_38

action_102 (163) = happyShift action_90
action_102 (167) = happyShift action_9
action_102 (172) = happyShift action_10
action_102 (173) = happyShift action_25
action_102 (180) = happyShift action_26
action_102 (183) = happyShift action_91
action_102 (184) = happyShift action_92
action_102 (185) = happyShift action_93
action_102 (186) = happyShift action_27
action_102 (187) = happyShift action_94
action_102 (188) = happyShift action_95
action_102 (189) = happyShift action_28
action_102 (191) = happyShift action_29
action_102 (192) = happyShift action_96
action_102 (24) = happyGoto action_106
action_102 (26) = happyGoto action_99
action_102 (27) = happyGoto action_100
action_102 (30) = happyGoto action_101
action_102 (31) = happyGoto action_51
action_102 (32) = happyGoto action_52
action_102 (33) = happyGoto action_53
action_102 (34) = happyGoto action_54
action_102 (38) = happyGoto action_55
action_102 (39) = happyGoto action_56
action_102 (40) = happyGoto action_57
action_102 (41) = happyGoto action_58
action_102 (42) = happyGoto action_59
action_102 (43) = happyGoto action_60
action_102 (44) = happyGoto action_61
action_102 (45) = happyGoto action_62
action_102 (46) = happyGoto action_63
action_102 (47) = happyGoto action_64
action_102 (48) = happyGoto action_65
action_102 (49) = happyGoto action_66
action_102 (50) = happyGoto action_67
action_102 (51) = happyGoto action_68
action_102 (52) = happyGoto action_69
action_102 (53) = happyGoto action_70
action_102 (92) = happyGoto action_71
action_102 (93) = happyGoto action_72
action_102 (94) = happyGoto action_73
action_102 (95) = happyGoto action_74
action_102 (96) = happyGoto action_75
action_102 (97) = happyGoto action_76
action_102 (99) = happyGoto action_17
action_102 (119) = happyGoto action_103
action_102 (120) = happyGoto action_18
action_102 (121) = happyGoto action_19
action_102 (122) = happyGoto action_20
action_102 (123) = happyGoto action_21
action_102 (124) = happyGoto action_22
action_102 (125) = happyGoto action_81
action_102 (126) = happyGoto action_82
action_102 (127) = happyGoto action_83
action_102 (128) = happyGoto action_84
action_102 (129) = happyGoto action_23
action_102 (130) = happyGoto action_85
action_102 (131) = happyGoto action_86
action_102 (132) = happyGoto action_87
action_102 _ = happyFail

action_103 (167) = happyShift action_9
action_103 (172) = happyShift action_10
action_103 (173) = happyShift action_25
action_103 (180) = happyShift action_26
action_103 (183) = happyShift action_91
action_103 (184) = happyShift action_92
action_103 (185) = happyShift action_93
action_103 (186) = happyShift action_27
action_103 (187) = happyShift action_94
action_103 (188) = happyShift action_95
action_103 (189) = happyShift action_28
action_103 (191) = happyShift action_29
action_103 (192) = happyShift action_96
action_103 (30) = happyGoto action_104
action_103 (31) = happyGoto action_51
action_103 (32) = happyGoto action_52
action_103 (33) = happyGoto action_53
action_103 (34) = happyGoto action_54
action_103 (38) = happyGoto action_55
action_103 (39) = happyGoto action_105
action_103 (40) = happyGoto action_57
action_103 (41) = happyGoto action_58
action_103 (42) = happyGoto action_59
action_103 (43) = happyGoto action_60
action_103 (44) = happyGoto action_61
action_103 (45) = happyGoto action_62
action_103 (46) = happyGoto action_63
action_103 (47) = happyGoto action_64
action_103 (48) = happyGoto action_65
action_103 (49) = happyGoto action_66
action_103 (50) = happyGoto action_67
action_103 (51) = happyGoto action_68
action_103 (52) = happyGoto action_69
action_103 (53) = happyGoto action_70
action_103 (92) = happyGoto action_71
action_103 (93) = happyGoto action_72
action_103 (94) = happyGoto action_73
action_103 (95) = happyGoto action_74
action_103 (96) = happyGoto action_75
action_103 (97) = happyGoto action_76
action_103 (99) = happyGoto action_17
action_103 (120) = happyGoto action_18
action_103 (121) = happyGoto action_19
action_103 (122) = happyGoto action_20
action_103 (123) = happyGoto action_21
action_103 (124) = happyGoto action_22
action_103 (125) = happyGoto action_81
action_103 (126) = happyGoto action_82
action_103 (127) = happyGoto action_83
action_103 (128) = happyGoto action_84
action_103 (129) = happyGoto action_23
action_103 (130) = happyGoto action_85
action_103 (131) = happyGoto action_86
action_103 (132) = happyGoto action_87
action_103 _ = happyFail

action_104 _ = happyReduce_39

action_105 (152) = happyShift action_123
action_105 (35) = happyGoto action_118
action_105 (36) = happyGoto action_119
action_105 (113) = happyGoto action_121
action_105 _ = happyFail

action_106 (151) = happyShift action_145
action_106 (101) = happyGoto action_183
action_106 _ = happyFail

action_107 _ = happyReduce_35

action_108 (163) = happyShift action_90
action_108 (167) = happyShift action_9
action_108 (172) = happyShift action_10
action_108 (173) = happyShift action_25
action_108 (180) = happyShift action_26
action_108 (183) = happyShift action_91
action_108 (184) = happyShift action_92
action_108 (185) = happyShift action_93
action_108 (186) = happyShift action_27
action_108 (187) = happyShift action_94
action_108 (188) = happyShift action_95
action_108 (189) = happyShift action_28
action_108 (191) = happyShift action_29
action_108 (192) = happyShift action_96
action_108 (26) = happyGoto action_182
action_108 (27) = happyGoto action_100
action_108 (30) = happyGoto action_101
action_108 (31) = happyGoto action_51
action_108 (32) = happyGoto action_52
action_108 (33) = happyGoto action_53
action_108 (34) = happyGoto action_54
action_108 (38) = happyGoto action_55
action_108 (39) = happyGoto action_56
action_108 (40) = happyGoto action_57
action_108 (41) = happyGoto action_58
action_108 (42) = happyGoto action_59
action_108 (43) = happyGoto action_60
action_108 (44) = happyGoto action_61
action_108 (45) = happyGoto action_62
action_108 (46) = happyGoto action_63
action_108 (47) = happyGoto action_64
action_108 (48) = happyGoto action_65
action_108 (49) = happyGoto action_66
action_108 (50) = happyGoto action_67
action_108 (51) = happyGoto action_68
action_108 (52) = happyGoto action_69
action_108 (53) = happyGoto action_70
action_108 (92) = happyGoto action_71
action_108 (93) = happyGoto action_72
action_108 (94) = happyGoto action_73
action_108 (95) = happyGoto action_74
action_108 (96) = happyGoto action_75
action_108 (97) = happyGoto action_76
action_108 (99) = happyGoto action_17
action_108 (119) = happyGoto action_103
action_108 (120) = happyGoto action_18
action_108 (121) = happyGoto action_19
action_108 (122) = happyGoto action_20
action_108 (123) = happyGoto action_21
action_108 (124) = happyGoto action_22
action_108 (125) = happyGoto action_81
action_108 (126) = happyGoto action_82
action_108 (127) = happyGoto action_83
action_108 (128) = happyGoto action_84
action_108 (129) = happyGoto action_23
action_108 (130) = happyGoto action_85
action_108 (131) = happyGoto action_86
action_108 (132) = happyGoto action_87
action_108 _ = happyFail

action_109 _ = happyReduce_189

action_110 (151) = happyShift action_145
action_110 (101) = happyGoto action_181
action_110 _ = happyFail

action_111 (167) = happyShift action_9
action_111 (168) = happyShift action_176
action_111 (171) = happyShift action_177
action_111 (172) = happyShift action_10
action_111 (173) = happyShift action_25
action_111 (174) = happyShift action_178
action_111 (175) = happyShift action_179
action_111 (179) = happyShift action_180
action_111 (180) = happyShift action_26
action_111 (186) = happyShift action_27
action_111 (189) = happyShift action_28
action_111 (191) = happyShift action_29
action_111 (55) = happyGoto action_162
action_111 (56) = happyGoto action_163
action_111 (57) = happyGoto action_164
action_111 (62) = happyGoto action_165
action_111 (64) = happyGoto action_166
action_111 (65) = happyGoto action_167
action_111 (67) = happyGoto action_168
action_111 (69) = happyGoto action_169
action_111 (91) = happyGoto action_170
action_111 (92) = happyGoto action_16
action_111 (99) = happyGoto action_17
action_111 (120) = happyGoto action_18
action_111 (121) = happyGoto action_19
action_111 (122) = happyGoto action_20
action_111 (123) = happyGoto action_21
action_111 (124) = happyGoto action_22
action_111 (129) = happyGoto action_23
action_111 (131) = happyGoto action_24
action_111 (133) = happyGoto action_171
action_111 (134) = happyGoto action_172
action_111 (135) = happyGoto action_173
action_111 (136) = happyGoto action_174
action_111 (139) = happyGoto action_175
action_111 _ = happyFail

action_112 _ = happyReduce_32

action_113 (151) = happyShift action_145
action_113 (101) = happyGoto action_161
action_113 _ = happyFail

action_114 (191) = happyShift action_29
action_114 (131) = happyGoto action_160
action_114 _ = happyFail

action_115 (167) = happyShift action_9
action_115 (172) = happyShift action_10
action_115 (173) = happyShift action_25
action_115 (180) = happyShift action_26
action_115 (183) = happyShift action_91
action_115 (184) = happyShift action_92
action_115 (185) = happyShift action_93
action_115 (186) = happyShift action_27
action_115 (187) = happyShift action_94
action_115 (188) = happyShift action_95
action_115 (189) = happyShift action_28
action_115 (191) = happyShift action_29
action_115 (192) = happyShift action_96
action_115 (39) = happyGoto action_156
action_115 (40) = happyGoto action_57
action_115 (41) = happyGoto action_152
action_115 (42) = happyGoto action_59
action_115 (43) = happyGoto action_60
action_115 (44) = happyGoto action_61
action_115 (45) = happyGoto action_62
action_115 (46) = happyGoto action_63
action_115 (47) = happyGoto action_153
action_115 (48) = happyGoto action_65
action_115 (49) = happyGoto action_66
action_115 (50) = happyGoto action_154
action_115 (51) = happyGoto action_68
action_115 (52) = happyGoto action_69
action_115 (53) = happyGoto action_70
action_115 (54) = happyGoto action_159
action_115 (92) = happyGoto action_71
action_115 (93) = happyGoto action_72
action_115 (94) = happyGoto action_73
action_115 (95) = happyGoto action_74
action_115 (96) = happyGoto action_75
action_115 (97) = happyGoto action_76
action_115 (99) = happyGoto action_17
action_115 (120) = happyGoto action_18
action_115 (121) = happyGoto action_19
action_115 (122) = happyGoto action_20
action_115 (123) = happyGoto action_21
action_115 (124) = happyGoto action_22
action_115 (125) = happyGoto action_81
action_115 (126) = happyGoto action_82
action_115 (127) = happyGoto action_83
action_115 (128) = happyGoto action_84
action_115 (129) = happyGoto action_23
action_115 (130) = happyGoto action_85
action_115 (131) = happyGoto action_86
action_115 (132) = happyGoto action_87
action_115 _ = happyFail

action_116 (167) = happyShift action_9
action_116 (172) = happyShift action_10
action_116 (173) = happyShift action_25
action_116 (180) = happyShift action_26
action_116 (183) = happyShift action_91
action_116 (184) = happyShift action_92
action_116 (185) = happyShift action_93
action_116 (186) = happyShift action_27
action_116 (187) = happyShift action_94
action_116 (188) = happyShift action_95
action_116 (189) = happyShift action_28
action_116 (191) = happyShift action_29
action_116 (192) = happyShift action_96
action_116 (39) = happyGoto action_156
action_116 (40) = happyGoto action_57
action_116 (41) = happyGoto action_152
action_116 (42) = happyGoto action_59
action_116 (43) = happyGoto action_60
action_116 (44) = happyGoto action_61
action_116 (45) = happyGoto action_62
action_116 (46) = happyGoto action_63
action_116 (47) = happyGoto action_153
action_116 (48) = happyGoto action_65
action_116 (49) = happyGoto action_66
action_116 (50) = happyGoto action_154
action_116 (51) = happyGoto action_68
action_116 (52) = happyGoto action_69
action_116 (53) = happyGoto action_70
action_116 (54) = happyGoto action_158
action_116 (92) = happyGoto action_71
action_116 (93) = happyGoto action_72
action_116 (94) = happyGoto action_73
action_116 (95) = happyGoto action_74
action_116 (96) = happyGoto action_75
action_116 (97) = happyGoto action_76
action_116 (99) = happyGoto action_17
action_116 (120) = happyGoto action_18
action_116 (121) = happyGoto action_19
action_116 (122) = happyGoto action_20
action_116 (123) = happyGoto action_21
action_116 (124) = happyGoto action_22
action_116 (125) = happyGoto action_81
action_116 (126) = happyGoto action_82
action_116 (127) = happyGoto action_83
action_116 (128) = happyGoto action_84
action_116 (129) = happyGoto action_23
action_116 (130) = happyGoto action_85
action_116 (131) = happyGoto action_86
action_116 (132) = happyGoto action_87
action_116 _ = happyFail

action_117 (167) = happyShift action_9
action_117 (172) = happyShift action_10
action_117 (173) = happyShift action_25
action_117 (180) = happyShift action_26
action_117 (183) = happyShift action_91
action_117 (184) = happyShift action_92
action_117 (185) = happyShift action_93
action_117 (186) = happyShift action_27
action_117 (187) = happyShift action_94
action_117 (188) = happyShift action_95
action_117 (189) = happyShift action_28
action_117 (191) = happyShift action_29
action_117 (192) = happyShift action_96
action_117 (39) = happyGoto action_156
action_117 (40) = happyGoto action_57
action_117 (41) = happyGoto action_152
action_117 (42) = happyGoto action_59
action_117 (43) = happyGoto action_60
action_117 (44) = happyGoto action_61
action_117 (45) = happyGoto action_62
action_117 (46) = happyGoto action_63
action_117 (47) = happyGoto action_153
action_117 (48) = happyGoto action_65
action_117 (49) = happyGoto action_66
action_117 (50) = happyGoto action_154
action_117 (51) = happyGoto action_68
action_117 (52) = happyGoto action_69
action_117 (53) = happyGoto action_70
action_117 (54) = happyGoto action_157
action_117 (92) = happyGoto action_71
action_117 (93) = happyGoto action_72
action_117 (94) = happyGoto action_73
action_117 (95) = happyGoto action_74
action_117 (96) = happyGoto action_75
action_117 (97) = happyGoto action_76
action_117 (99) = happyGoto action_17
action_117 (120) = happyGoto action_18
action_117 (121) = happyGoto action_19
action_117 (122) = happyGoto action_20
action_117 (123) = happyGoto action_21
action_117 (124) = happyGoto action_22
action_117 (125) = happyGoto action_81
action_117 (126) = happyGoto action_82
action_117 (127) = happyGoto action_83
action_117 (128) = happyGoto action_84
action_117 (129) = happyGoto action_23
action_117 (130) = happyGoto action_85
action_117 (131) = happyGoto action_86
action_117 (132) = happyGoto action_87
action_117 _ = happyFail

action_118 (167) = happyShift action_9
action_118 (172) = happyShift action_10
action_118 (173) = happyShift action_25
action_118 (180) = happyShift action_26
action_118 (183) = happyShift action_91
action_118 (184) = happyShift action_92
action_118 (185) = happyShift action_93
action_118 (186) = happyShift action_27
action_118 (187) = happyShift action_94
action_118 (188) = happyShift action_95
action_118 (189) = happyShift action_28
action_118 (191) = happyShift action_29
action_118 (192) = happyShift action_96
action_118 (39) = happyGoto action_155
action_118 (40) = happyGoto action_57
action_118 (41) = happyGoto action_152
action_118 (42) = happyGoto action_59
action_118 (43) = happyGoto action_60
action_118 (44) = happyGoto action_61
action_118 (45) = happyGoto action_62
action_118 (46) = happyGoto action_63
action_118 (47) = happyGoto action_153
action_118 (48) = happyGoto action_65
action_118 (49) = happyGoto action_66
action_118 (50) = happyGoto action_154
action_118 (51) = happyGoto action_68
action_118 (52) = happyGoto action_69
action_118 (53) = happyGoto action_70
action_118 (92) = happyGoto action_71
action_118 (93) = happyGoto action_72
action_118 (94) = happyGoto action_73
action_118 (95) = happyGoto action_74
action_118 (96) = happyGoto action_75
action_118 (97) = happyGoto action_76
action_118 (99) = happyGoto action_17
action_118 (120) = happyGoto action_18
action_118 (121) = happyGoto action_19
action_118 (122) = happyGoto action_20
action_118 (123) = happyGoto action_21
action_118 (124) = happyGoto action_22
action_118 (125) = happyGoto action_81
action_118 (126) = happyGoto action_82
action_118 (127) = happyGoto action_83
action_118 (128) = happyGoto action_84
action_118 (129) = happyGoto action_23
action_118 (130) = happyGoto action_85
action_118 (131) = happyGoto action_86
action_118 (132) = happyGoto action_87
action_118 _ = happyFail

action_119 _ = happyReduce_58

action_120 (167) = happyShift action_9
action_120 (172) = happyShift action_10
action_120 (173) = happyShift action_25
action_120 (180) = happyShift action_26
action_120 (183) = happyShift action_91
action_120 (184) = happyShift action_92
action_120 (185) = happyShift action_93
action_120 (186) = happyShift action_27
action_120 (187) = happyShift action_94
action_120 (188) = happyShift action_95
action_120 (189) = happyShift action_28
action_120 (191) = happyShift action_29
action_120 (192) = happyShift action_96
action_120 (39) = happyGoto action_151
action_120 (40) = happyGoto action_57
action_120 (41) = happyGoto action_152
action_120 (42) = happyGoto action_59
action_120 (43) = happyGoto action_60
action_120 (44) = happyGoto action_61
action_120 (45) = happyGoto action_62
action_120 (46) = happyGoto action_63
action_120 (47) = happyGoto action_153
action_120 (48) = happyGoto action_65
action_120 (49) = happyGoto action_66
action_120 (50) = happyGoto action_154
action_120 (51) = happyGoto action_68
action_120 (52) = happyGoto action_69
action_120 (53) = happyGoto action_70
action_120 (92) = happyGoto action_71
action_120 (93) = happyGoto action_72
action_120 (94) = happyGoto action_73
action_120 (95) = happyGoto action_74
action_120 (96) = happyGoto action_75
action_120 (97) = happyGoto action_76
action_120 (99) = happyGoto action_17
action_120 (120) = happyGoto action_18
action_120 (121) = happyGoto action_19
action_120 (122) = happyGoto action_20
action_120 (123) = happyGoto action_21
action_120 (124) = happyGoto action_22
action_120 (125) = happyGoto action_81
action_120 (126) = happyGoto action_82
action_120 (127) = happyGoto action_83
action_120 (128) = happyGoto action_84
action_120 (129) = happyGoto action_23
action_120 (130) = happyGoto action_85
action_120 (131) = happyGoto action_86
action_120 (132) = happyGoto action_87
action_120 _ = happyFail

action_121 _ = happyReduce_59

action_122 _ = happyReduce_60

action_123 _ = happyReduce_184

action_124 _ = happyReduce_185

action_125 (192) = happyShift action_96
action_125 (21) = happyGoto action_149
action_125 (53) = happyGoto action_150
action_125 (128) = happyGoto action_84
action_125 _ = happyFail

action_126 _ = happyReduce_173

action_127 (149) = happyShift action_12
action_127 (161) = happyShift action_88
action_127 (162) = happyShift action_89
action_127 (163) = happyShift action_90
action_127 (167) = happyShift action_9
action_127 (172) = happyShift action_10
action_127 (173) = happyShift action_25
action_127 (180) = happyShift action_26
action_127 (183) = happyShift action_91
action_127 (184) = happyShift action_92
action_127 (185) = happyShift action_93
action_127 (186) = happyShift action_27
action_127 (187) = happyShift action_94
action_127 (188) = happyShift action_95
action_127 (189) = happyShift action_28
action_127 (191) = happyShift action_29
action_127 (192) = happyShift action_96
action_127 (19) = happyGoto action_148
action_127 (20) = happyGoto action_46
action_127 (22) = happyGoto action_47
action_127 (27) = happyGoto action_48
action_127 (28) = happyGoto action_49
action_127 (30) = happyGoto action_50
action_127 (31) = happyGoto action_51
action_127 (32) = happyGoto action_52
action_127 (33) = happyGoto action_53
action_127 (34) = happyGoto action_54
action_127 (38) = happyGoto action_55
action_127 (39) = happyGoto action_56
action_127 (40) = happyGoto action_57
action_127 (41) = happyGoto action_58
action_127 (42) = happyGoto action_59
action_127 (43) = happyGoto action_60
action_127 (44) = happyGoto action_61
action_127 (45) = happyGoto action_62
action_127 (46) = happyGoto action_63
action_127 (47) = happyGoto action_64
action_127 (48) = happyGoto action_65
action_127 (49) = happyGoto action_66
action_127 (50) = happyGoto action_67
action_127 (51) = happyGoto action_68
action_127 (52) = happyGoto action_69
action_127 (53) = happyGoto action_70
action_127 (92) = happyGoto action_71
action_127 (93) = happyGoto action_72
action_127 (94) = happyGoto action_73
action_127 (95) = happyGoto action_74
action_127 (96) = happyGoto action_75
action_127 (97) = happyGoto action_76
action_127 (99) = happyGoto action_17
action_127 (100) = happyGoto action_77
action_127 (115) = happyGoto action_78
action_127 (116) = happyGoto action_79
action_127 (119) = happyGoto action_80
action_127 (120) = happyGoto action_18
action_127 (121) = happyGoto action_19
action_127 (122) = happyGoto action_20
action_127 (123) = happyGoto action_21
action_127 (124) = happyGoto action_22
action_127 (125) = happyGoto action_81
action_127 (126) = happyGoto action_82
action_127 (127) = happyGoto action_83
action_127 (128) = happyGoto action_84
action_127 (129) = happyGoto action_23
action_127 (130) = happyGoto action_85
action_127 (131) = happyGoto action_86
action_127 (132) = happyGoto action_87
action_127 _ = happyFail

action_128 _ = happyReduce_44

action_129 _ = happyReduce_46

action_130 _ = happyReduce_49

action_131 _ = happyReduce_48

action_132 _ = happyReduce_47

action_133 _ = happyReduce_45

action_134 (149) = happyShift action_12
action_134 (161) = happyShift action_88
action_134 (162) = happyShift action_89
action_134 (163) = happyShift action_90
action_134 (167) = happyShift action_9
action_134 (172) = happyShift action_10
action_134 (173) = happyShift action_25
action_134 (180) = happyShift action_26
action_134 (183) = happyShift action_91
action_134 (184) = happyShift action_92
action_134 (185) = happyShift action_93
action_134 (186) = happyShift action_27
action_134 (187) = happyShift action_94
action_134 (188) = happyShift action_95
action_134 (189) = happyShift action_28
action_134 (191) = happyShift action_29
action_134 (192) = happyShift action_96
action_134 (19) = happyGoto action_147
action_134 (20) = happyGoto action_46
action_134 (22) = happyGoto action_47
action_134 (27) = happyGoto action_48
action_134 (28) = happyGoto action_49
action_134 (30) = happyGoto action_50
action_134 (31) = happyGoto action_51
action_134 (32) = happyGoto action_52
action_134 (33) = happyGoto action_53
action_134 (34) = happyGoto action_54
action_134 (38) = happyGoto action_55
action_134 (39) = happyGoto action_56
action_134 (40) = happyGoto action_57
action_134 (41) = happyGoto action_58
action_134 (42) = happyGoto action_59
action_134 (43) = happyGoto action_60
action_134 (44) = happyGoto action_61
action_134 (45) = happyGoto action_62
action_134 (46) = happyGoto action_63
action_134 (47) = happyGoto action_64
action_134 (48) = happyGoto action_65
action_134 (49) = happyGoto action_66
action_134 (50) = happyGoto action_67
action_134 (51) = happyGoto action_68
action_134 (52) = happyGoto action_69
action_134 (53) = happyGoto action_70
action_134 (92) = happyGoto action_71
action_134 (93) = happyGoto action_72
action_134 (94) = happyGoto action_73
action_134 (95) = happyGoto action_74
action_134 (96) = happyGoto action_75
action_134 (97) = happyGoto action_76
action_134 (99) = happyGoto action_17
action_134 (100) = happyGoto action_77
action_134 (115) = happyGoto action_78
action_134 (116) = happyGoto action_79
action_134 (119) = happyGoto action_80
action_134 (120) = happyGoto action_18
action_134 (121) = happyGoto action_19
action_134 (122) = happyGoto action_20
action_134 (123) = happyGoto action_21
action_134 (124) = happyGoto action_22
action_134 (125) = happyGoto action_81
action_134 (126) = happyGoto action_82
action_134 (127) = happyGoto action_83
action_134 (128) = happyGoto action_84
action_134 (129) = happyGoto action_23
action_134 (130) = happyGoto action_85
action_134 (131) = happyGoto action_86
action_134 (132) = happyGoto action_87
action_134 _ = happyFail

action_135 (149) = happyShift action_12
action_135 (161) = happyShift action_88
action_135 (162) = happyShift action_89
action_135 (163) = happyShift action_90
action_135 (167) = happyShift action_9
action_135 (172) = happyShift action_10
action_135 (173) = happyShift action_25
action_135 (180) = happyShift action_26
action_135 (183) = happyShift action_91
action_135 (184) = happyShift action_92
action_135 (185) = happyShift action_93
action_135 (186) = happyShift action_27
action_135 (187) = happyShift action_94
action_135 (188) = happyShift action_95
action_135 (189) = happyShift action_28
action_135 (191) = happyShift action_29
action_135 (192) = happyShift action_96
action_135 (19) = happyGoto action_146
action_135 (20) = happyGoto action_46
action_135 (22) = happyGoto action_47
action_135 (27) = happyGoto action_48
action_135 (28) = happyGoto action_49
action_135 (30) = happyGoto action_50
action_135 (31) = happyGoto action_51
action_135 (32) = happyGoto action_52
action_135 (33) = happyGoto action_53
action_135 (34) = happyGoto action_54
action_135 (38) = happyGoto action_55
action_135 (39) = happyGoto action_56
action_135 (40) = happyGoto action_57
action_135 (41) = happyGoto action_58
action_135 (42) = happyGoto action_59
action_135 (43) = happyGoto action_60
action_135 (44) = happyGoto action_61
action_135 (45) = happyGoto action_62
action_135 (46) = happyGoto action_63
action_135 (47) = happyGoto action_64
action_135 (48) = happyGoto action_65
action_135 (49) = happyGoto action_66
action_135 (50) = happyGoto action_67
action_135 (51) = happyGoto action_68
action_135 (52) = happyGoto action_69
action_135 (53) = happyGoto action_70
action_135 (92) = happyGoto action_71
action_135 (93) = happyGoto action_72
action_135 (94) = happyGoto action_73
action_135 (95) = happyGoto action_74
action_135 (96) = happyGoto action_75
action_135 (97) = happyGoto action_76
action_135 (99) = happyGoto action_17
action_135 (100) = happyGoto action_77
action_135 (115) = happyGoto action_78
action_135 (116) = happyGoto action_79
action_135 (119) = happyGoto action_80
action_135 (120) = happyGoto action_18
action_135 (121) = happyGoto action_19
action_135 (122) = happyGoto action_20
action_135 (123) = happyGoto action_21
action_135 (124) = happyGoto action_22
action_135 (125) = happyGoto action_81
action_135 (126) = happyGoto action_82
action_135 (127) = happyGoto action_83
action_135 (128) = happyGoto action_84
action_135 (129) = happyGoto action_23
action_135 (130) = happyGoto action_85
action_135 (131) = happyGoto action_86
action_135 (132) = happyGoto action_87
action_135 _ = happyFail

action_136 _ = happyReduce_178

action_137 _ = happyReduce_183

action_138 _ = happyReduce_179

action_139 _ = happyReduce_182

action_140 _ = happyReduce_181

action_141 _ = happyReduce_180

action_142 _ = happyReduce_188

action_143 (151) = happyShift action_145
action_143 (101) = happyGoto action_144
action_143 _ = happyFail

action_144 (147) = happyShift action_186
action_144 (105) = happyGoto action_205
action_144 _ = happyFail

action_145 _ = happyReduce_172

action_146 (164) = happyShift action_109
action_146 (16) = happyGoto action_203
action_146 (118) = happyGoto action_204
action_146 _ = happyReduce_19

action_147 (160) = happyShift action_142
action_147 (18) = happyGoto action_201
action_147 (117) = happyGoto action_202
action_147 _ = happyReduce_22

action_148 _ = happyReduce_15

action_149 (150) = happyShift action_200
action_149 (103) = happyGoto action_199
action_149 _ = happyFail

action_150 (146) = happyShift action_32
action_150 (104) = happyGoto action_198
action_150 _ = happyReduce_29

action_151 _ = happyReduce_41

action_152 _ = happyReduce_65

action_153 _ = happyReduce_75

action_154 _ = happyReduce_66

action_155 _ = happyReduce_57

action_156 (146) = happyShift action_32
action_156 (104) = happyGoto action_197
action_156 _ = happyReduce_85

action_157 (151) = happyShift action_145
action_157 (101) = happyGoto action_196
action_157 _ = happyFail

action_158 (151) = happyShift action_145
action_158 (101) = happyGoto action_195
action_158 _ = happyFail

action_159 (151) = happyShift action_145
action_159 (101) = happyGoto action_194
action_159 _ = happyFail

action_160 _ = happyReduce_164

action_161 _ = happyReduce_25

action_162 (146) = happyShift action_32
action_162 (71) = happyGoto action_192
action_162 (104) = happyGoto action_193
action_162 _ = happyReduce_113

action_163 _ = happyReduce_87

action_164 _ = happyReduce_91

action_165 _ = happyReduce_88

action_166 _ = happyReduce_89

action_167 _ = happyReduce_101

action_168 _ = happyReduce_102

action_169 _ = happyReduce_103

action_170 _ = happyReduce_90

action_171 (149) = happyShift action_12
action_171 (100) = happyGoto action_191
action_171 _ = happyFail

action_172 (149) = happyShift action_12
action_172 (100) = happyGoto action_190
action_172 _ = happyFail

action_173 (149) = happyShift action_12
action_173 (100) = happyGoto action_189
action_173 _ = happyFail

action_174 (149) = happyShift action_12
action_174 (100) = happyGoto action_188
action_174 _ = happyFail

action_175 (149) = happyShift action_12
action_175 (100) = happyGoto action_187
action_175 _ = happyFail

action_176 _ = happyReduce_210

action_177 _ = happyReduce_205

action_178 _ = happyReduce_204

action_179 _ = happyReduce_207

action_180 _ = happyReduce_206

action_181 (147) = happyShift action_186
action_181 (105) = happyGoto action_185
action_181 _ = happyFail

action_182 (164) = happyShift action_109
action_182 (25) = happyGoto action_184
action_182 (118) = happyGoto action_108
action_182 _ = happyReduce_36

action_183 _ = happyReduce_34

action_184 _ = happyReduce_37

action_185 _ = happyReduce_7

action_186 _ = happyReduce_176

action_187 (167) = happyShift action_9
action_187 (172) = happyShift action_10
action_187 (173) = happyShift action_25
action_187 (180) = happyShift action_26
action_187 (186) = happyShift action_27
action_187 (189) = happyShift action_28
action_187 (70) = happyGoto action_225
action_187 (92) = happyGoto action_226
action_187 (99) = happyGoto action_17
action_187 (120) = happyGoto action_18
action_187 (121) = happyGoto action_19
action_187 (122) = happyGoto action_20
action_187 (123) = happyGoto action_21
action_187 (124) = happyGoto action_22
action_187 (129) = happyGoto action_23
action_187 _ = happyFail

action_188 (167) = happyShift action_9
action_188 (172) = happyShift action_10
action_188 (173) = happyShift action_25
action_188 (180) = happyShift action_26
action_188 (186) = happyShift action_27
action_188 (63) = happyGoto action_223
action_188 (99) = happyGoto action_224
action_188 (120) = happyGoto action_18
action_188 (121) = happyGoto action_19
action_188 (122) = happyGoto action_20
action_188 (123) = happyGoto action_21
action_188 (129) = happyGoto action_23
action_188 _ = happyFail

action_189 (165) = happyShift action_221
action_189 (170) = happyShift action_222
action_189 (68) = happyGoto action_218
action_189 (137) = happyGoto action_219
action_189 (138) = happyGoto action_220
action_189 _ = happyFail

action_190 (189) = happyShift action_28
action_190 (98) = happyGoto action_216
action_190 (124) = happyGoto action_217
action_190 _ = happyFail

action_191 (167) = happyShift action_9
action_191 (172) = happyShift action_10
action_191 (173) = happyShift action_25
action_191 (180) = happyShift action_26
action_191 (186) = happyShift action_27
action_191 (189) = happyShift action_28
action_191 (58) = happyGoto action_214
action_191 (92) = happyGoto action_215
action_191 (99) = happyGoto action_17
action_191 (120) = happyGoto action_18
action_191 (121) = happyGoto action_19
action_191 (122) = happyGoto action_20
action_191 (123) = happyGoto action_21
action_191 (124) = happyGoto action_22
action_191 (129) = happyGoto action_23
action_191 _ = happyFail

action_192 _ = happyReduce_8

action_193 (148) = happyShift action_126
action_193 (72) = happyGoto action_212
action_193 (102) = happyGoto action_213
action_193 _ = happyFail

action_194 _ = happyReduce_81

action_195 _ = happyReduce_77

action_196 _ = happyReduce_68

action_197 (167) = happyShift action_9
action_197 (172) = happyShift action_10
action_197 (173) = happyShift action_25
action_197 (180) = happyShift action_26
action_197 (183) = happyShift action_91
action_197 (184) = happyShift action_92
action_197 (185) = happyShift action_93
action_197 (186) = happyShift action_27
action_197 (187) = happyShift action_94
action_197 (188) = happyShift action_95
action_197 (189) = happyShift action_28
action_197 (191) = happyShift action_29
action_197 (192) = happyShift action_96
action_197 (39) = happyGoto action_156
action_197 (40) = happyGoto action_57
action_197 (41) = happyGoto action_152
action_197 (42) = happyGoto action_59
action_197 (43) = happyGoto action_60
action_197 (44) = happyGoto action_61
action_197 (45) = happyGoto action_62
action_197 (46) = happyGoto action_63
action_197 (47) = happyGoto action_153
action_197 (48) = happyGoto action_65
action_197 (49) = happyGoto action_66
action_197 (50) = happyGoto action_154
action_197 (51) = happyGoto action_68
action_197 (52) = happyGoto action_69
action_197 (53) = happyGoto action_70
action_197 (54) = happyGoto action_211
action_197 (92) = happyGoto action_71
action_197 (93) = happyGoto action_72
action_197 (94) = happyGoto action_73
action_197 (95) = happyGoto action_74
action_197 (96) = happyGoto action_75
action_197 (97) = happyGoto action_76
action_197 (99) = happyGoto action_17
action_197 (120) = happyGoto action_18
action_197 (121) = happyGoto action_19
action_197 (122) = happyGoto action_20
action_197 (123) = happyGoto action_21
action_197 (124) = happyGoto action_22
action_197 (125) = happyGoto action_81
action_197 (126) = happyGoto action_82
action_197 (127) = happyGoto action_83
action_197 (128) = happyGoto action_84
action_197 (129) = happyGoto action_23
action_197 (130) = happyGoto action_85
action_197 (131) = happyGoto action_86
action_197 (132) = happyGoto action_87
action_197 _ = happyFail

action_198 (192) = happyShift action_96
action_198 (21) = happyGoto action_210
action_198 (53) = happyGoto action_150
action_198 (128) = happyGoto action_84
action_198 _ = happyFail

action_199 (145) = happyShift action_209
action_199 (106) = happyGoto action_208
action_199 _ = happyFail

action_200 _ = happyReduce_174

action_201 _ = happyReduce_21

action_202 (149) = happyShift action_12
action_202 (161) = happyShift action_88
action_202 (162) = happyShift action_89
action_202 (163) = happyShift action_90
action_202 (167) = happyShift action_9
action_202 (172) = happyShift action_10
action_202 (173) = happyShift action_25
action_202 (180) = happyShift action_26
action_202 (183) = happyShift action_91
action_202 (184) = happyShift action_92
action_202 (185) = happyShift action_93
action_202 (186) = happyShift action_27
action_202 (187) = happyShift action_94
action_202 (188) = happyShift action_95
action_202 (189) = happyShift action_28
action_202 (191) = happyShift action_29
action_202 (192) = happyShift action_96
action_202 (19) = happyGoto action_207
action_202 (20) = happyGoto action_46
action_202 (22) = happyGoto action_47
action_202 (27) = happyGoto action_48
action_202 (28) = happyGoto action_49
action_202 (30) = happyGoto action_50
action_202 (31) = happyGoto action_51
action_202 (32) = happyGoto action_52
action_202 (33) = happyGoto action_53
action_202 (34) = happyGoto action_54
action_202 (38) = happyGoto action_55
action_202 (39) = happyGoto action_56
action_202 (40) = happyGoto action_57
action_202 (41) = happyGoto action_58
action_202 (42) = happyGoto action_59
action_202 (43) = happyGoto action_60
action_202 (44) = happyGoto action_61
action_202 (45) = happyGoto action_62
action_202 (46) = happyGoto action_63
action_202 (47) = happyGoto action_64
action_202 (48) = happyGoto action_65
action_202 (49) = happyGoto action_66
action_202 (50) = happyGoto action_67
action_202 (51) = happyGoto action_68
action_202 (52) = happyGoto action_69
action_202 (53) = happyGoto action_70
action_202 (92) = happyGoto action_71
action_202 (93) = happyGoto action_72
action_202 (94) = happyGoto action_73
action_202 (95) = happyGoto action_74
action_202 (96) = happyGoto action_75
action_202 (97) = happyGoto action_76
action_202 (99) = happyGoto action_17
action_202 (100) = happyGoto action_77
action_202 (115) = happyGoto action_78
action_202 (116) = happyGoto action_79
action_202 (119) = happyGoto action_80
action_202 (120) = happyGoto action_18
action_202 (121) = happyGoto action_19
action_202 (122) = happyGoto action_20
action_202 (123) = happyGoto action_21
action_202 (124) = happyGoto action_22
action_202 (125) = happyGoto action_81
action_202 (126) = happyGoto action_82
action_202 (127) = happyGoto action_83
action_202 (128) = happyGoto action_84
action_202 (129) = happyGoto action_23
action_202 (130) = happyGoto action_85
action_202 (131) = happyGoto action_86
action_202 (132) = happyGoto action_87
action_202 _ = happyFail

action_203 _ = happyReduce_18

action_204 (149) = happyShift action_12
action_204 (161) = happyShift action_88
action_204 (162) = happyShift action_89
action_204 (163) = happyShift action_90
action_204 (167) = happyShift action_9
action_204 (172) = happyShift action_10
action_204 (173) = happyShift action_25
action_204 (180) = happyShift action_26
action_204 (183) = happyShift action_91
action_204 (184) = happyShift action_92
action_204 (185) = happyShift action_93
action_204 (186) = happyShift action_27
action_204 (187) = happyShift action_94
action_204 (188) = happyShift action_95
action_204 (189) = happyShift action_28
action_204 (191) = happyShift action_29
action_204 (192) = happyShift action_96
action_204 (19) = happyGoto action_206
action_204 (20) = happyGoto action_46
action_204 (22) = happyGoto action_47
action_204 (27) = happyGoto action_48
action_204 (28) = happyGoto action_49
action_204 (30) = happyGoto action_50
action_204 (31) = happyGoto action_51
action_204 (32) = happyGoto action_52
action_204 (33) = happyGoto action_53
action_204 (34) = happyGoto action_54
action_204 (38) = happyGoto action_55
action_204 (39) = happyGoto action_56
action_204 (40) = happyGoto action_57
action_204 (41) = happyGoto action_58
action_204 (42) = happyGoto action_59
action_204 (43) = happyGoto action_60
action_204 (44) = happyGoto action_61
action_204 (45) = happyGoto action_62
action_204 (46) = happyGoto action_63
action_204 (47) = happyGoto action_64
action_204 (48) = happyGoto action_65
action_204 (49) = happyGoto action_66
action_204 (50) = happyGoto action_67
action_204 (51) = happyGoto action_68
action_204 (52) = happyGoto action_69
action_204 (53) = happyGoto action_70
action_204 (92) = happyGoto action_71
action_204 (93) = happyGoto action_72
action_204 (94) = happyGoto action_73
action_204 (95) = happyGoto action_74
action_204 (96) = happyGoto action_75
action_204 (97) = happyGoto action_76
action_204 (99) = happyGoto action_17
action_204 (100) = happyGoto action_77
action_204 (115) = happyGoto action_78
action_204 (116) = happyGoto action_79
action_204 (119) = happyGoto action_80
action_204 (120) = happyGoto action_18
action_204 (121) = happyGoto action_19
action_204 (122) = happyGoto action_20
action_204 (123) = happyGoto action_21
action_204 (124) = happyGoto action_22
action_204 (125) = happyGoto action_81
action_204 (126) = happyGoto action_82
action_204 (127) = happyGoto action_83
action_204 (128) = happyGoto action_84
action_204 (129) = happyGoto action_23
action_204 (130) = happyGoto action_85
action_204 (131) = happyGoto action_86
action_204 (132) = happyGoto action_87
action_204 _ = happyFail

action_205 _ = happyReduce_6

action_206 (164) = happyShift action_109
action_206 (16) = happyGoto action_259
action_206 (118) = happyGoto action_204
action_206 _ = happyReduce_19

action_207 (160) = happyShift action_142
action_207 (18) = happyGoto action_258
action_207 (117) = happyGoto action_202
action_207 _ = happyReduce_22

action_208 (149) = happyShift action_12
action_208 (161) = happyShift action_88
action_208 (162) = happyShift action_89
action_208 (163) = happyShift action_90
action_208 (167) = happyShift action_9
action_208 (172) = happyShift action_10
action_208 (173) = happyShift action_25
action_208 (180) = happyShift action_26
action_208 (183) = happyShift action_91
action_208 (184) = happyShift action_92
action_208 (185) = happyShift action_93
action_208 (186) = happyShift action_27
action_208 (187) = happyShift action_94
action_208 (188) = happyShift action_95
action_208 (189) = happyShift action_28
action_208 (191) = happyShift action_29
action_208 (192) = happyShift action_96
action_208 (19) = happyGoto action_257
action_208 (20) = happyGoto action_46
action_208 (22) = happyGoto action_47
action_208 (27) = happyGoto action_48
action_208 (28) = happyGoto action_49
action_208 (30) = happyGoto action_50
action_208 (31) = happyGoto action_51
action_208 (32) = happyGoto action_52
action_208 (33) = happyGoto action_53
action_208 (34) = happyGoto action_54
action_208 (38) = happyGoto action_55
action_208 (39) = happyGoto action_56
action_208 (40) = happyGoto action_57
action_208 (41) = happyGoto action_58
action_208 (42) = happyGoto action_59
action_208 (43) = happyGoto action_60
action_208 (44) = happyGoto action_61
action_208 (45) = happyGoto action_62
action_208 (46) = happyGoto action_63
action_208 (47) = happyGoto action_64
action_208 (48) = happyGoto action_65
action_208 (49) = happyGoto action_66
action_208 (50) = happyGoto action_67
action_208 (51) = happyGoto action_68
action_208 (52) = happyGoto action_69
action_208 (53) = happyGoto action_70
action_208 (92) = happyGoto action_71
action_208 (93) = happyGoto action_72
action_208 (94) = happyGoto action_73
action_208 (95) = happyGoto action_74
action_208 (96) = happyGoto action_75
action_208 (97) = happyGoto action_76
action_208 (99) = happyGoto action_17
action_208 (100) = happyGoto action_77
action_208 (115) = happyGoto action_78
action_208 (116) = happyGoto action_79
action_208 (119) = happyGoto action_80
action_208 (120) = happyGoto action_18
action_208 (121) = happyGoto action_19
action_208 (122) = happyGoto action_20
action_208 (123) = happyGoto action_21
action_208 (124) = happyGoto action_22
action_208 (125) = happyGoto action_81
action_208 (126) = happyGoto action_82
action_208 (127) = happyGoto action_83
action_208 (128) = happyGoto action_84
action_208 (129) = happyGoto action_23
action_208 (130) = happyGoto action_85
action_208 (131) = happyGoto action_86
action_208 (132) = happyGoto action_87
action_208 _ = happyFail

action_209 _ = happyReduce_177

action_210 _ = happyReduce_30

action_211 _ = happyReduce_86

action_212 _ = happyReduce_112

action_213 (150) = happyShift action_200
action_213 (166) = happyShift action_252
action_213 (167) = happyShift action_9
action_213 (169) = happyShift action_253
action_213 (172) = happyShift action_10
action_213 (173) = happyShift action_25
action_213 (176) = happyShift action_254
action_213 (177) = happyShift action_255
action_213 (178) = happyShift action_256
action_213 (180) = happyShift action_26
action_213 (186) = happyShift action_27
action_213 (189) = happyShift action_28
action_213 (58) = happyGoto action_233
action_213 (73) = happyGoto action_234
action_213 (74) = happyGoto action_235
action_213 (75) = happyGoto action_236
action_213 (76) = happyGoto action_237
action_213 (77) = happyGoto action_238
action_213 (78) = happyGoto action_239
action_213 (79) = happyGoto action_240
action_213 (80) = happyGoto action_241
action_213 (82) = happyGoto action_242
action_213 (83) = happyGoto action_243
action_213 (84) = happyGoto action_244
action_213 (92) = happyGoto action_245
action_213 (99) = happyGoto action_17
action_213 (103) = happyGoto action_246
action_213 (120) = happyGoto action_18
action_213 (121) = happyGoto action_19
action_213 (122) = happyGoto action_20
action_213 (123) = happyGoto action_21
action_213 (124) = happyGoto action_22
action_213 (129) = happyGoto action_23
action_213 (140) = happyGoto action_247
action_213 (141) = happyGoto action_248
action_213 (142) = happyGoto action_249
action_213 (143) = happyGoto action_250
action_213 (144) = happyGoto action_251
action_213 _ = happyFail

action_214 (146) = happyShift action_32
action_214 (104) = happyGoto action_232
action_214 _ = happyFail

action_215 _ = happyReduce_93

action_216 (146) = happyShift action_32
action_216 (66) = happyGoto action_230
action_216 (104) = happyGoto action_231
action_216 _ = happyReduce_106

action_217 _ = happyReduce_165

action_218 (146) = happyShift action_32
action_218 (71) = happyGoto action_229
action_218 (104) = happyGoto action_193
action_218 _ = happyReduce_113

action_219 _ = happyReduce_109

action_220 _ = happyReduce_108

action_221 _ = happyReduce_208

action_222 _ = happyReduce_209

action_223 (146) = happyShift action_32
action_223 (71) = happyGoto action_228
action_223 (104) = happyGoto action_193
action_223 _ = happyReduce_113

action_224 _ = happyReduce_100

action_225 (146) = happyShift action_32
action_225 (71) = happyGoto action_227
action_225 (104) = happyGoto action_193
action_225 _ = happyReduce_113

action_226 _ = happyReduce_111

action_227 (151) = happyShift action_145
action_227 (101) = happyGoto action_274
action_227 _ = happyFail

action_228 (151) = happyShift action_145
action_228 (101) = happyGoto action_273
action_228 _ = happyFail

action_229 (151) = happyShift action_145
action_229 (101) = happyGoto action_272
action_229 _ = happyFail

action_230 (151) = happyShift action_145
action_230 (101) = happyGoto action_271
action_230 _ = happyFail

action_231 (167) = happyShift action_9
action_231 (172) = happyShift action_10
action_231 (173) = happyShift action_25
action_231 (180) = happyShift action_26
action_231 (186) = happyShift action_27
action_231 (189) = happyShift action_28
action_231 (191) = happyShift action_29
action_231 (91) = happyGoto action_270
action_231 (92) = happyGoto action_16
action_231 (99) = happyGoto action_17
action_231 (120) = happyGoto action_18
action_231 (121) = happyGoto action_19
action_231 (122) = happyGoto action_20
action_231 (123) = happyGoto action_21
action_231 (124) = happyGoto action_22
action_231 (129) = happyGoto action_23
action_231 (131) = happyGoto action_24
action_231 _ = happyFail

action_232 (148) = happyShift action_126
action_232 (72) = happyGoto action_269
action_232 (102) = happyGoto action_213
action_232 _ = happyFail

action_233 (149) = happyShift action_12
action_233 (100) = happyGoto action_268
action_233 _ = happyFail

action_234 (150) = happyShift action_200
action_234 (103) = happyGoto action_267
action_234 _ = happyFail

action_235 (146) = happyShift action_32
action_235 (104) = happyGoto action_266
action_235 _ = happyReduce_116

action_236 _ = happyReduce_120

action_237 _ = happyReduce_118

action_238 _ = happyReduce_122

action_239 _ = happyReduce_123

action_240 _ = happyReduce_119

action_241 _ = happyReduce_126

action_242 _ = happyReduce_130

action_243 _ = happyReduce_127

action_244 _ = happyReduce_128

action_245 (149) = happyShift action_12
action_245 (100) = happyGoto action_265
action_245 _ = happyFail

action_246 _ = happyReduce_115

action_247 (149) = happyShift action_12
action_247 (100) = happyGoto action_264
action_247 _ = happyFail

action_248 (149) = happyShift action_12
action_248 (100) = happyGoto action_263
action_248 _ = happyFail

action_249 (149) = happyShift action_12
action_249 (100) = happyGoto action_262
action_249 _ = happyFail

action_250 (149) = happyShift action_12
action_250 (100) = happyGoto action_261
action_250 _ = happyFail

action_251 (149) = happyShift action_12
action_251 (100) = happyGoto action_260
action_251 _ = happyFail

action_252 _ = happyReduce_213

action_253 _ = happyReduce_215

action_254 _ = happyReduce_211

action_255 _ = happyReduce_214

action_256 _ = happyReduce_212

action_257 _ = happyReduce_28

action_258 _ = happyReduce_23

action_259 _ = happyReduce_20

action_260 (167) = happyShift action_9
action_260 (172) = happyShift action_10
action_260 (173) = happyShift action_25
action_260 (180) = happyShift action_26
action_260 (186) = happyShift action_27
action_260 (189) = happyShift action_28
action_260 (92) = happyGoto action_296
action_260 (99) = happyGoto action_17
action_260 (120) = happyGoto action_18
action_260 (121) = happyGoto action_19
action_260 (122) = happyGoto action_20
action_260 (123) = happyGoto action_21
action_260 (124) = happyGoto action_22
action_260 (129) = happyGoto action_23
action_260 _ = happyFail

action_261 (171) = happyShift action_177
action_261 (65) = happyGoto action_295
action_261 (134) = happyGoto action_172
action_261 _ = happyFail

action_262 (148) = happyShift action_126
action_262 (102) = happyGoto action_294
action_262 _ = happyFail

action_263 (167) = happyShift action_9
action_263 (172) = happyShift action_10
action_263 (173) = happyShift action_25
action_263 (180) = happyShift action_26
action_263 (186) = happyShift action_27
action_263 (81) = happyGoto action_292
action_263 (99) = happyGoto action_293
action_263 (120) = happyGoto action_18
action_263 (121) = happyGoto action_19
action_263 (122) = happyGoto action_20
action_263 (123) = happyGoto action_21
action_263 (129) = happyGoto action_23
action_263 _ = happyFail

action_264 (167) = happyShift action_9
action_264 (172) = happyShift action_10
action_264 (173) = happyShift action_25
action_264 (180) = happyShift action_26
action_264 (186) = happyShift action_27
action_264 (189) = happyShift action_28
action_264 (92) = happyGoto action_291
action_264 (99) = happyGoto action_17
action_264 (120) = happyGoto action_18
action_264 (121) = happyGoto action_19
action_264 (122) = happyGoto action_20
action_264 (123) = happyGoto action_21
action_264 (124) = happyGoto action_22
action_264 (129) = happyGoto action_23
action_264 _ = happyFail

action_265 (148) = happyShift action_126
action_265 (167) = happyShift action_9
action_265 (172) = happyShift action_10
action_265 (173) = happyShift action_25
action_265 (180) = happyShift action_288
action_265 (181) = happyShift action_289
action_265 (182) = happyShift action_290
action_265 (183) = happyShift action_91
action_265 (186) = happyShift action_27
action_265 (187) = happyShift action_94
action_265 (188) = happyShift action_95
action_265 (189) = happyShift action_28
action_265 (191) = happyShift action_29
action_265 (192) = happyShift action_96
action_265 (53) = happyGoto action_278
action_265 (86) = happyGoto action_279
action_265 (87) = happyGoto action_280
action_265 (88) = happyGoto action_281
action_265 (89) = happyGoto action_282
action_265 (90) = happyGoto action_283
action_265 (92) = happyGoto action_284
action_265 (95) = happyGoto action_285
action_265 (96) = happyGoto action_75
action_265 (97) = happyGoto action_76
action_265 (99) = happyGoto action_17
action_265 (102) = happyGoto action_286
action_265 (120) = happyGoto action_18
action_265 (121) = happyGoto action_19
action_265 (122) = happyGoto action_20
action_265 (123) = happyGoto action_21
action_265 (124) = happyGoto action_22
action_265 (125) = happyGoto action_287
action_265 (128) = happyGoto action_84
action_265 (129) = happyGoto action_23
action_265 (130) = happyGoto action_85
action_265 (131) = happyGoto action_86
action_265 (132) = happyGoto action_87
action_265 _ = happyFail

action_266 (166) = happyShift action_252
action_266 (167) = happyShift action_9
action_266 (169) = happyShift action_253
action_266 (172) = happyShift action_10
action_266 (173) = happyShift action_25
action_266 (176) = happyShift action_254
action_266 (177) = happyShift action_255
action_266 (178) = happyShift action_256
action_266 (180) = happyShift action_26
action_266 (186) = happyShift action_27
action_266 (189) = happyShift action_28
action_266 (58) = happyGoto action_233
action_266 (73) = happyGoto action_277
action_266 (74) = happyGoto action_235
action_266 (75) = happyGoto action_236
action_266 (76) = happyGoto action_237
action_266 (77) = happyGoto action_238
action_266 (78) = happyGoto action_239
action_266 (79) = happyGoto action_240
action_266 (80) = happyGoto action_241
action_266 (82) = happyGoto action_242
action_266 (83) = happyGoto action_243
action_266 (84) = happyGoto action_244
action_266 (92) = happyGoto action_245
action_266 (99) = happyGoto action_17
action_266 (120) = happyGoto action_18
action_266 (121) = happyGoto action_19
action_266 (122) = happyGoto action_20
action_266 (123) = happyGoto action_21
action_266 (124) = happyGoto action_22
action_266 (129) = happyGoto action_23
action_266 (140) = happyGoto action_247
action_266 (141) = happyGoto action_248
action_266 (142) = happyGoto action_249
action_266 (143) = happyGoto action_250
action_266 (144) = happyGoto action_251
action_266 _ = happyFail

action_267 _ = happyReduce_114

action_268 (167) = happyShift action_9
action_268 (172) = happyShift action_10
action_268 (173) = happyShift action_25
action_268 (180) = happyShift action_26
action_268 (186) = happyShift action_27
action_268 (189) = happyShift action_28
action_268 (92) = happyGoto action_276
action_268 (99) = happyGoto action_17
action_268 (120) = happyGoto action_18
action_268 (121) = happyGoto action_19
action_268 (122) = happyGoto action_20
action_268 (123) = happyGoto action_21
action_268 (124) = happyGoto action_22
action_268 (129) = happyGoto action_23
action_268 _ = happyFail

action_269 (146) = happyShift action_32
action_269 (104) = happyGoto action_275
action_269 _ = happyFail

action_270 _ = happyReduce_105

action_271 _ = happyReduce_104

action_272 _ = happyReduce_107

action_273 _ = happyReduce_99

action_274 _ = happyReduce_110

action_275 (148) = happyShift action_126
action_275 (102) = happyGoto action_313
action_275 _ = happyFail

action_276 (146) = happyShift action_32
action_276 (104) = happyGoto action_312
action_276 _ = happyFail

action_277 _ = happyReduce_117

action_278 _ = happyReduce_145

action_279 (146) = happyShift action_32
action_279 (104) = happyGoto action_311
action_279 _ = happyReduce_151

action_280 (145) = happyShift action_209
action_280 (106) = happyGoto action_310
action_280 _ = happyReduce_137

action_281 _ = happyReduce_143

action_282 _ = happyReduce_139

action_283 (151) = happyShift action_145
action_283 (101) = happyGoto action_309
action_283 _ = happyFail

action_284 (149) = happyShift action_12
action_284 (100) = happyGoto action_308
action_284 _ = happyReduce_141

action_285 _ = happyReduce_144

action_286 (148) = happyShift action_126
action_286 (150) = happyShift action_200
action_286 (167) = happyShift action_9
action_286 (172) = happyShift action_10
action_286 (173) = happyShift action_25
action_286 (180) = happyShift action_288
action_286 (181) = happyShift action_289
action_286 (182) = happyShift action_290
action_286 (183) = happyShift action_91
action_286 (186) = happyShift action_27
action_286 (187) = happyShift action_94
action_286 (188) = happyShift action_95
action_286 (189) = happyShift action_28
action_286 (191) = happyShift action_29
action_286 (192) = happyShift action_96
action_286 (53) = happyGoto action_278
action_286 (86) = happyGoto action_279
action_286 (87) = happyGoto action_280
action_286 (88) = happyGoto action_281
action_286 (89) = happyGoto action_282
action_286 (90) = happyGoto action_306
action_286 (92) = happyGoto action_284
action_286 (95) = happyGoto action_285
action_286 (96) = happyGoto action_75
action_286 (97) = happyGoto action_76
action_286 (99) = happyGoto action_17
action_286 (102) = happyGoto action_286
action_286 (103) = happyGoto action_307
action_286 (120) = happyGoto action_18
action_286 (121) = happyGoto action_19
action_286 (122) = happyGoto action_20
action_286 (123) = happyGoto action_21
action_286 (124) = happyGoto action_22
action_286 (125) = happyGoto action_287
action_286 (128) = happyGoto action_84
action_286 (129) = happyGoto action_23
action_286 (130) = happyGoto action_85
action_286 (131) = happyGoto action_86
action_286 (132) = happyGoto action_87
action_286 _ = happyFail

action_287 _ = happyReduce_142

action_288 (149) = happyShift action_12
action_288 (100) = happyGoto action_305
action_288 _ = happyReduce_193

action_289 (149) = happyShift action_12
action_289 (100) = happyGoto action_304
action_289 _ = happyFail

action_290 (149) = happyShift action_12
action_290 (100) = happyGoto action_303
action_290 _ = happyFail

action_291 (151) = happyShift action_145
action_291 (101) = happyGoto action_302
action_291 _ = happyFail

action_292 (151) = happyShift action_145
action_292 (101) = happyGoto action_301
action_292 _ = happyFail

action_293 _ = happyReduce_131

action_294 (167) = happyShift action_9
action_294 (172) = happyShift action_10
action_294 (173) = happyShift action_25
action_294 (180) = happyShift action_26
action_294 (186) = happyShift action_27
action_294 (189) = happyShift action_28
action_294 (191) = happyShift action_29
action_294 (85) = happyGoto action_299
action_294 (91) = happyGoto action_300
action_294 (92) = happyGoto action_16
action_294 (99) = happyGoto action_17
action_294 (120) = happyGoto action_18
action_294 (121) = happyGoto action_19
action_294 (122) = happyGoto action_20
action_294 (123) = happyGoto action_21
action_294 (124) = happyGoto action_22
action_294 (129) = happyGoto action_23
action_294 (131) = happyGoto action_24
action_294 _ = happyFail

action_295 (151) = happyShift action_145
action_295 (101) = happyGoto action_298
action_295 _ = happyFail

action_296 (151) = happyShift action_145
action_296 (101) = happyGoto action_297
action_296 _ = happyFail

action_297 _ = happyReduce_124

action_298 _ = happyReduce_134

action_299 (150) = happyShift action_200
action_299 (103) = happyGoto action_326
action_299 _ = happyFail

action_300 (146) = happyShift action_32
action_300 (104) = happyGoto action_325
action_300 _ = happyReduce_135

action_301 _ = happyReduce_129

action_302 _ = happyReduce_125

action_303 (167) = happyShift action_9
action_303 (172) = happyShift action_10
action_303 (173) = happyShift action_25
action_303 (180) = happyShift action_26
action_303 (183) = happyShift action_91
action_303 (184) = happyShift action_92
action_303 (185) = happyShift action_93
action_303 (186) = happyShift action_27
action_303 (187) = happyShift action_94
action_303 (188) = happyShift action_95
action_303 (189) = happyShift action_28
action_303 (191) = happyShift action_29
action_303 (192) = happyShift action_96
action_303 (39) = happyGoto action_324
action_303 (40) = happyGoto action_57
action_303 (41) = happyGoto action_152
action_303 (42) = happyGoto action_59
action_303 (43) = happyGoto action_60
action_303 (44) = happyGoto action_61
action_303 (45) = happyGoto action_62
action_303 (46) = happyGoto action_63
action_303 (47) = happyGoto action_153
action_303 (48) = happyGoto action_65
action_303 (49) = happyGoto action_66
action_303 (50) = happyGoto action_154
action_303 (51) = happyGoto action_68
action_303 (52) = happyGoto action_69
action_303 (53) = happyGoto action_70
action_303 (92) = happyGoto action_71
action_303 (93) = happyGoto action_72
action_303 (94) = happyGoto action_73
action_303 (95) = happyGoto action_74
action_303 (96) = happyGoto action_75
action_303 (97) = happyGoto action_76
action_303 (99) = happyGoto action_17
action_303 (120) = happyGoto action_18
action_303 (121) = happyGoto action_19
action_303 (122) = happyGoto action_20
action_303 (123) = happyGoto action_21
action_303 (124) = happyGoto action_22
action_303 (125) = happyGoto action_81
action_303 (126) = happyGoto action_82
action_303 (127) = happyGoto action_83
action_303 (128) = happyGoto action_84
action_303 (129) = happyGoto action_23
action_303 (130) = happyGoto action_85
action_303 (131) = happyGoto action_86
action_303 (132) = happyGoto action_87
action_303 _ = happyFail

action_304 (149) = happyShift action_12
action_304 (161) = happyShift action_88
action_304 (162) = happyShift action_89
action_304 (163) = happyShift action_90
action_304 (167) = happyShift action_9
action_304 (172) = happyShift action_10
action_304 (173) = happyShift action_25
action_304 (180) = happyShift action_26
action_304 (183) = happyShift action_91
action_304 (184) = happyShift action_92
action_304 (185) = happyShift action_93
action_304 (186) = happyShift action_27
action_304 (187) = happyShift action_94
action_304 (188) = happyShift action_95
action_304 (189) = happyShift action_28
action_304 (191) = happyShift action_29
action_304 (192) = happyShift action_96
action_304 (11) = happyGoto action_323
action_304 (12) = happyGoto action_40
action_304 (13) = happyGoto action_41
action_304 (14) = happyGoto action_42
action_304 (15) = happyGoto action_43
action_304 (17) = happyGoto action_44
action_304 (19) = happyGoto action_45
action_304 (20) = happyGoto action_46
action_304 (22) = happyGoto action_47
action_304 (27) = happyGoto action_48
action_304 (28) = happyGoto action_49
action_304 (30) = happyGoto action_50
action_304 (31) = happyGoto action_51
action_304 (32) = happyGoto action_52
action_304 (33) = happyGoto action_53
action_304 (34) = happyGoto action_54
action_304 (38) = happyGoto action_55
action_304 (39) = happyGoto action_56
action_304 (40) = happyGoto action_57
action_304 (41) = happyGoto action_58
action_304 (42) = happyGoto action_59
action_304 (43) = happyGoto action_60
action_304 (44) = happyGoto action_61
action_304 (45) = happyGoto action_62
action_304 (46) = happyGoto action_63
action_304 (47) = happyGoto action_64
action_304 (48) = happyGoto action_65
action_304 (49) = happyGoto action_66
action_304 (50) = happyGoto action_67
action_304 (51) = happyGoto action_68
action_304 (52) = happyGoto action_69
action_304 (53) = happyGoto action_70
action_304 (92) = happyGoto action_71
action_304 (93) = happyGoto action_72
action_304 (94) = happyGoto action_73
action_304 (95) = happyGoto action_74
action_304 (96) = happyGoto action_75
action_304 (97) = happyGoto action_76
action_304 (99) = happyGoto action_17
action_304 (100) = happyGoto action_77
action_304 (115) = happyGoto action_78
action_304 (116) = happyGoto action_79
action_304 (119) = happyGoto action_80
action_304 (120) = happyGoto action_18
action_304 (121) = happyGoto action_19
action_304 (122) = happyGoto action_20
action_304 (123) = happyGoto action_21
action_304 (124) = happyGoto action_22
action_304 (125) = happyGoto action_81
action_304 (126) = happyGoto action_82
action_304 (127) = happyGoto action_83
action_304 (128) = happyGoto action_84
action_304 (129) = happyGoto action_23
action_304 (130) = happyGoto action_85
action_304 (131) = happyGoto action_86
action_304 (132) = happyGoto action_87
action_304 _ = happyFail

action_305 (149) = happyShift action_12
action_305 (163) = happyShift action_90
action_305 (167) = happyShift action_9
action_305 (172) = happyShift action_10
action_305 (173) = happyShift action_25
action_305 (180) = happyShift action_26
action_305 (183) = happyShift action_91
action_305 (184) = happyShift action_92
action_305 (185) = happyShift action_93
action_305 (186) = happyShift action_27
action_305 (187) = happyShift action_94
action_305 (188) = happyShift action_95
action_305 (189) = happyShift action_28
action_305 (191) = happyShift action_29
action_305 (192) = happyShift action_96
action_305 (23) = happyGoto action_322
action_305 (24) = happyGoto action_98
action_305 (26) = happyGoto action_99
action_305 (27) = happyGoto action_100
action_305 (30) = happyGoto action_101
action_305 (31) = happyGoto action_51
action_305 (32) = happyGoto action_52
action_305 (33) = happyGoto action_53
action_305 (34) = happyGoto action_54
action_305 (38) = happyGoto action_55
action_305 (39) = happyGoto action_56
action_305 (40) = happyGoto action_57
action_305 (41) = happyGoto action_58
action_305 (42) = happyGoto action_59
action_305 (43) = happyGoto action_60
action_305 (44) = happyGoto action_61
action_305 (45) = happyGoto action_62
action_305 (46) = happyGoto action_63
action_305 (47) = happyGoto action_64
action_305 (48) = happyGoto action_65
action_305 (49) = happyGoto action_66
action_305 (50) = happyGoto action_67
action_305 (51) = happyGoto action_68
action_305 (52) = happyGoto action_69
action_305 (53) = happyGoto action_70
action_305 (92) = happyGoto action_71
action_305 (93) = happyGoto action_72
action_305 (94) = happyGoto action_73
action_305 (95) = happyGoto action_74
action_305 (96) = happyGoto action_75
action_305 (97) = happyGoto action_76
action_305 (99) = happyGoto action_17
action_305 (100) = happyGoto action_102
action_305 (119) = happyGoto action_103
action_305 (120) = happyGoto action_18
action_305 (121) = happyGoto action_19
action_305 (122) = happyGoto action_20
action_305 (123) = happyGoto action_21
action_305 (124) = happyGoto action_22
action_305 (125) = happyGoto action_81
action_305 (126) = happyGoto action_82
action_305 (127) = happyGoto action_83
action_305 (128) = happyGoto action_84
action_305 (129) = happyGoto action_23
action_305 (130) = happyGoto action_85
action_305 (131) = happyGoto action_86
action_305 (132) = happyGoto action_87
action_305 _ = happyFail

action_306 (150) = happyShift action_200
action_306 (103) = happyGoto action_321
action_306 _ = happyFail

action_307 _ = happyReduce_149

action_308 (148) = happyShift action_126
action_308 (167) = happyShift action_9
action_308 (172) = happyShift action_10
action_308 (173) = happyShift action_25
action_308 (180) = happyShift action_288
action_308 (181) = happyShift action_289
action_308 (182) = happyShift action_290
action_308 (183) = happyShift action_91
action_308 (186) = happyShift action_27
action_308 (187) = happyShift action_94
action_308 (188) = happyShift action_95
action_308 (189) = happyShift action_28
action_308 (191) = happyShift action_29
action_308 (192) = happyShift action_96
action_308 (53) = happyGoto action_278
action_308 (86) = happyGoto action_279
action_308 (87) = happyGoto action_280
action_308 (88) = happyGoto action_281
action_308 (89) = happyGoto action_282
action_308 (90) = happyGoto action_320
action_308 (92) = happyGoto action_284
action_308 (95) = happyGoto action_285
action_308 (96) = happyGoto action_75
action_308 (97) = happyGoto action_76
action_308 (99) = happyGoto action_17
action_308 (102) = happyGoto action_286
action_308 (120) = happyGoto action_18
action_308 (121) = happyGoto action_19
action_308 (122) = happyGoto action_20
action_308 (123) = happyGoto action_21
action_308 (124) = happyGoto action_22
action_308 (125) = happyGoto action_287
action_308 (128) = happyGoto action_84
action_308 (129) = happyGoto action_23
action_308 (130) = happyGoto action_85
action_308 (131) = happyGoto action_86
action_308 (132) = happyGoto action_87
action_308 _ = happyFail

action_309 _ = happyReduce_121

action_310 (148) = happyShift action_126
action_310 (167) = happyShift action_9
action_310 (172) = happyShift action_10
action_310 (173) = happyShift action_25
action_310 (180) = happyShift action_288
action_310 (181) = happyShift action_289
action_310 (182) = happyShift action_290
action_310 (183) = happyShift action_91
action_310 (186) = happyShift action_27
action_310 (187) = happyShift action_94
action_310 (188) = happyShift action_95
action_310 (189) = happyShift action_28
action_310 (191) = happyShift action_29
action_310 (192) = happyShift action_96
action_310 (53) = happyGoto action_278
action_310 (86) = happyGoto action_319
action_310 (87) = happyGoto action_280
action_310 (88) = happyGoto action_281
action_310 (89) = happyGoto action_282
action_310 (92) = happyGoto action_284
action_310 (95) = happyGoto action_285
action_310 (96) = happyGoto action_75
action_310 (97) = happyGoto action_76
action_310 (99) = happyGoto action_17
action_310 (102) = happyGoto action_286
action_310 (120) = happyGoto action_18
action_310 (121) = happyGoto action_19
action_310 (122) = happyGoto action_20
action_310 (123) = happyGoto action_21
action_310 (124) = happyGoto action_22
action_310 (125) = happyGoto action_287
action_310 (128) = happyGoto action_84
action_310 (129) = happyGoto action_23
action_310 (130) = happyGoto action_85
action_310 (131) = happyGoto action_86
action_310 (132) = happyGoto action_87
action_310 _ = happyFail

action_311 (148) = happyShift action_126
action_311 (167) = happyShift action_9
action_311 (172) = happyShift action_10
action_311 (173) = happyShift action_25
action_311 (180) = happyShift action_288
action_311 (181) = happyShift action_289
action_311 (182) = happyShift action_290
action_311 (183) = happyShift action_91
action_311 (186) = happyShift action_27
action_311 (187) = happyShift action_94
action_311 (188) = happyShift action_95
action_311 (189) = happyShift action_28
action_311 (191) = happyShift action_29
action_311 (192) = happyShift action_96
action_311 (53) = happyGoto action_278
action_311 (86) = happyGoto action_279
action_311 (87) = happyGoto action_280
action_311 (88) = happyGoto action_281
action_311 (89) = happyGoto action_282
action_311 (90) = happyGoto action_318
action_311 (92) = happyGoto action_284
action_311 (95) = happyGoto action_285
action_311 (96) = happyGoto action_75
action_311 (97) = happyGoto action_76
action_311 (99) = happyGoto action_17
action_311 (102) = happyGoto action_286
action_311 (120) = happyGoto action_18
action_311 (121) = happyGoto action_19
action_311 (122) = happyGoto action_20
action_311 (123) = happyGoto action_21
action_311 (124) = happyGoto action_22
action_311 (125) = happyGoto action_287
action_311 (128) = happyGoto action_84
action_311 (129) = happyGoto action_23
action_311 (130) = happyGoto action_85
action_311 (131) = happyGoto action_86
action_311 (132) = happyGoto action_87
action_311 _ = happyFail

action_312 (148) = happyShift action_126
action_312 (89) = happyGoto action_317
action_312 (102) = happyGoto action_286
action_312 _ = happyFail

action_313 (167) = happyShift action_9
action_313 (172) = happyShift action_10
action_313 (173) = happyShift action_25
action_313 (180) = happyShift action_26
action_313 (186) = happyShift action_27
action_313 (189) = happyShift action_28
action_313 (59) = happyGoto action_314
action_313 (60) = happyGoto action_315
action_313 (92) = happyGoto action_316
action_313 (99) = happyGoto action_17
action_313 (120) = happyGoto action_18
action_313 (121) = happyGoto action_19
action_313 (122) = happyGoto action_20
action_313 (123) = happyGoto action_21
action_313 (124) = happyGoto action_22
action_313 (129) = happyGoto action_23
action_313 _ = happyFail

action_314 (150) = happyShift action_200
action_314 (103) = happyGoto action_337
action_314 _ = happyFail

action_315 (146) = happyShift action_32
action_315 (104) = happyGoto action_336
action_315 _ = happyReduce_94

action_316 (145) = happyShift action_209
action_316 (61) = happyGoto action_334
action_316 (106) = happyGoto action_335
action_316 _ = happyReduce_98

action_317 (151) = happyShift action_145
action_317 (101) = happyGoto action_333
action_317 _ = happyFail

action_318 _ = happyReduce_152

action_319 _ = happyReduce_138

action_320 (151) = happyShift action_145
action_320 (101) = happyGoto action_332
action_320 _ = happyFail

action_321 _ = happyReduce_150

action_322 (151) = happyShift action_145
action_322 (101) = happyGoto action_331
action_322 _ = happyFail

action_323 (151) = happyShift action_145
action_323 (101) = happyGoto action_330
action_323 _ = happyFail

action_324 (151) = happyShift action_145
action_324 (101) = happyGoto action_329
action_324 _ = happyFail

action_325 (167) = happyShift action_9
action_325 (172) = happyShift action_10
action_325 (173) = happyShift action_25
action_325 (180) = happyShift action_26
action_325 (186) = happyShift action_27
action_325 (189) = happyShift action_28
action_325 (191) = happyShift action_29
action_325 (85) = happyGoto action_328
action_325 (91) = happyGoto action_300
action_325 (92) = happyGoto action_16
action_325 (99) = happyGoto action_17
action_325 (120) = happyGoto action_18
action_325 (121) = happyGoto action_19
action_325 (122) = happyGoto action_20
action_325 (123) = happyGoto action_21
action_325 (124) = happyGoto action_22
action_325 (129) = happyGoto action_23
action_325 (131) = happyGoto action_24
action_325 _ = happyFail

action_326 (151) = happyShift action_145
action_326 (101) = happyGoto action_327
action_326 _ = happyFail

action_327 _ = happyReduce_133

action_328 _ = happyReduce_136

action_329 _ = happyReduce_148

action_330 _ = happyReduce_147

action_331 _ = happyReduce_146

action_332 _ = happyReduce_140

action_333 _ = happyReduce_132

action_334 _ = happyReduce_96

action_335 (148) = happyShift action_126
action_335 (89) = happyGoto action_340
action_335 (102) = happyGoto action_286
action_335 _ = happyFail

action_336 (167) = happyShift action_9
action_336 (172) = happyShift action_10
action_336 (173) = happyShift action_25
action_336 (180) = happyShift action_26
action_336 (186) = happyShift action_27
action_336 (189) = happyShift action_28
action_336 (59) = happyGoto action_339
action_336 (60) = happyGoto action_315
action_336 (92) = happyGoto action_316
action_336 (99) = happyGoto action_17
action_336 (120) = happyGoto action_18
action_336 (121) = happyGoto action_19
action_336 (122) = happyGoto action_20
action_336 (123) = happyGoto action_21
action_336 (124) = happyGoto action_22
action_336 (129) = happyGoto action_23
action_336 _ = happyFail

action_337 (151) = happyShift action_145
action_337 (101) = happyGoto action_338
action_337 _ = happyFail

action_338 _ = happyReduce_92

action_339 _ = happyReduce_95

action_340 _ = happyReduce_97

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 10 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_8) `HappyStk`
	(HappyAbsSyn11  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (F { formula = happy_var_7
                  , name    = readWord happy_var_3
                  , role    = happy_var_5
                  , source  = happy_var_8
                  }
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 10 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_8) `HappyStk`
	(HappyAbsSyn11  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (F { formula = happy_var_7
                  , name    = readWord happy_var_3
                  , role    = happy_var_5
                  , source  = happy_var_8
                  }
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  9 happyReduction_9
happyReduction_9  =  HappyAbsSyn9
		 (NoSource
	)

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn10
		 (readRole happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  11 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BinOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  14 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 15 happyReduction_18
happyReduction_18 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (L.foldl (binOp (:|:)) (BinOp happy_var_1 (:|:) happy_var_3) happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_0  16 happyReduction_19
happyReduction_19  =  HappyAbsSyn16
		 ([]
	)

happyReduce_20 = happySpecReduce_3  16 happyReduction_20
happyReduction_20 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 17 happyReduction_21
happyReduction_21 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (L.foldl (binOp (:&:)) (BinOp happy_var_1 (:&:) happy_var_3) happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_0  18 happyReduction_22
happyReduction_22  =  HappyAbsSyn16
		 ([]
	)

happyReduce_23 = happySpecReduce_3  18 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  19 happyReduction_24
happyReduction_24 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  19 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  19 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  19 happyReduction_27
happyReduction_27 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happyReduce 6 20 happyReduction_28
happyReduction_28 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Quant happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  21 happyReduction_29
happyReduction_29 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  21 happyReduction_30
happyReduction_30 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 : happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  22 happyReduction_31
happyReduction_31 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  22 happyReduction_32
happyReduction_32 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 ((:~:) happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  23 happyReduction_33
happyReduction_33 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  23 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  24 happyReduction_35
happyReduction_35 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (L.foldl (binOp (:|:)) happy_var_1 happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  25 happyReduction_36
happyReduction_36  =  HappyAbsSyn16
		 ([]
	)

happyReduce_37 = happySpecReduce_3  25 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  26 happyReduction_38
happyReduction_38 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  26 happyReduction_39
happyReduction_39 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 ((:~:) happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  26 happyReduction_40
happyReduction_40 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  27 happyReduction_41
happyReduction_41 (HappyAbsSyn39  happy_var_3)
	(HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn11
		 (InfixPred happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  28 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn28
		 (All
	)

happyReduce_43 = happySpecReduce_1  28 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn28
		 (Exists
	)

happyReduce_44 = happySpecReduce_1  29 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn29
		 ((:<=>:)
	)

happyReduce_45 = happySpecReduce_1  29 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn29
		 ((:<=:)
	)

happyReduce_46 = happySpecReduce_1  29 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn29
		 ((:=>:)
	)

happyReduce_47 = happySpecReduce_1  29 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn29
		 ((:~&:)
	)

happyReduce_48 = happySpecReduce_1  29 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn29
		 ((:~|:)
	)

happyReduce_49 = happySpecReduce_1  29 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn29
		 ((:<~>:)
	)

happyReduce_50 = happySpecReduce_1  30 happyReduction_50
happyReduction_50 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  30 happyReduction_51
happyReduction_51 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  30 happyReduction_52
happyReduction_52 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  31 happyReduction_53
happyReduction_53 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn11
		 (fApp2pApp happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  32 happyReduction_54
happyReduction_54 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  32 happyReduction_55
happyReduction_55 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  33 happyReduction_56
happyReduction_56 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn11
		 (fApp2pApp happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  34 happyReduction_57
happyReduction_57 (HappyAbsSyn39  happy_var_3)
	(HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn11
		 (InfixPred happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  35 happyReduction_58
happyReduction_58 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  36 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn35
		 ((:=:)
	)

happyReduce_60 = happySpecReduce_1  37 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn35
		 ((:!=:)
	)

happyReduce_61 = happySpecReduce_1  38 happyReduction_61
happyReduction_61 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn11
		 (fApp2pApp happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  39 happyReduction_62
happyReduction_62 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  39 happyReduction_63
happyReduction_63 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn39
		 (Var happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  40 happyReduction_64
happyReduction_64 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  40 happyReduction_65
happyReduction_65 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  40 happyReduction_66
happyReduction_66 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  41 happyReduction_67
happyReduction_67 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn39
		 (FunApp happy_var_1 []
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happyReduce 4 41 happyReduction_68
happyReduction_68 (_ `HappyStk`
	(HappyAbsSyn54  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (FunApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_1  42 happyReduction_69
happyReduction_69 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  43 happyReduction_70
happyReduction_70 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  44 happyReduction_71
happyReduction_71 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  44 happyReduction_72
happyReduction_72 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  45 happyReduction_73
happyReduction_73 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn39
		 (DistinctObjectTerm (stripQuotes '"' happy_var_1)
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  45 happyReduction_74
happyReduction_74 (HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn39
		 (NumberLitTerm happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  46 happyReduction_75
happyReduction_75 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  47 happyReduction_76
happyReduction_76 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn39
		 (FunApp (AtomicWord happy_var_1) []
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happyReduce 4 47 happyReduction_77
happyReduction_77 (_ `HappyStk`
	(HappyAbsSyn54  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (FunApp (AtomicWord happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_78 = happySpecReduce_1  48 happyReduction_78
happyReduction_78 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  49 happyReduction_79
happyReduction_79 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  50 happyReduction_80
happyReduction_80 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn39
		 (FunApp (AtomicWord happy_var_1) []
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happyReduce 4 50 happyReduction_81
happyReduction_81 (_ `HappyStk`
	(HappyAbsSyn54  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (FunApp (AtomicWord happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_82 = happySpecReduce_1  51 happyReduction_82
happyReduction_82 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  52 happyReduction_83
happyReduction_83 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  53 happyReduction_84
happyReduction_84 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn53
		 (V happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  54 happyReduction_85
happyReduction_85 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn54
		 ([happy_var_1]
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  54 happyReduction_86
happyReduction_86 (HappyAbsSyn54  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_1 : happy_var_3
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  55 happyReduction_87
happyReduction_87 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  55 happyReduction_88
happyReduction_88 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  55 happyReduction_89
happyReduction_89 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  56 happyReduction_90
happyReduction_90 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn9
		 (Name (readWord happy_var_1)
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  56 happyReduction_91
happyReduction_91 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happyReduce 10 57 happyReduction_92
happyReduction_92 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn59  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn71  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Inference happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_93 = happySpecReduce_1  58 happyReduction_93
happyReduction_93 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn58
		 (readRule $ readWord happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  59 happyReduction_94
happyReduction_94 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn59
		 ([happy_var_1]
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  59 happyReduction_95
happyReduction_95 (HappyAbsSyn59  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn59
		 (happy_var_1 : happy_var_3
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_2  60 happyReduction_96
happyReduction_96 (HappyAbsSyn61  happy_var_2)
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn60
		 (Parent (readWord happy_var_1) happy_var_2
	)
happyReduction_96 _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_2  61 happyReduction_97
happyReduction_97 (HappyAbsSyn61  happy_var_2)
	_
	 =  HappyAbsSyn61
		 (happy_var_2
	)
happyReduction_97 _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_0  61 happyReduction_98
happyReduction_98  =  HappyAbsSyn61
		 ([]
	)

happyReduce_99 = happyReduce 5 62 happyReduction_99
happyReduction_99 (_ `HappyStk`
	(HappyAbsSyn71  happy_var_4) `HappyStk`
	(HappyAbsSyn63  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Introduced happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_100 = happySpecReduce_1  63 happyReduction_100
happyReduction_100 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn63
		 (readType happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  64 happyReduction_101
happyReduction_101 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  64 happyReduction_102
happyReduction_102 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  64 happyReduction_103
happyReduction_103 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happyReduce 5 65 happyReduction_104
happyReduction_104 (_ `HappyStk`
	(HappyAbsSyn66  happy_var_4) `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (File    happy_var_3        happy_var_4
	) `HappyStk` happyRest

happyReduce_105 = happySpecReduce_2  66 happyReduction_105
happyReduction_105 (HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn66
		 (Just (readWord happy_var_2)
	)
happyReduction_105 _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_0  66 happyReduction_106
happyReduction_106  =  HappyAbsSyn66
		 (Nothing
	)

happyReduce_107 = happyReduce 5 67 happyReduction_107
happyReduction_107 (_ `HappyStk`
	(HappyAbsSyn71  happy_var_4) `HappyStk`
	(HappyAbsSyn68  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Theory    happy_var_3          happy_var_4
	) `HappyStk` happyRest

happyReduce_108 = happySpecReduce_1  68 happyReduction_108
happyReduction_108 _
	 =  HappyAbsSyn68
		 (Equality
	)

happyReduce_109 = happySpecReduce_1  68 happyReduction_109
happyReduction_109 _
	 =  HappyAbsSyn68
		 (AC
	)

happyReduce_110 = happyReduce 5 69 happyReduction_110
happyReduction_110 (_ `HappyStk`
	(HappyAbsSyn71  happy_var_4) `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Creator happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_111 = happySpecReduce_1  70 happyReduction_111
happyReduction_111 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn48
		 (readWord happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_2  71 happyReduction_112
happyReduction_112 (HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn71
		 (happy_var_2
	)
happyReduction_112 _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_0  71 happyReduction_113
happyReduction_113  =  HappyAbsSyn71
		 ([]
	)

happyReduce_114 = happySpecReduce_3  72 happyReduction_114
happyReduction_114 _
	(HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn71
		 (happy_var_2
	)
happyReduction_114 _ _ _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_2  72 happyReduction_115
happyReduction_115 _
	_
	 =  HappyAbsSyn71
		 ([]
	)

happyReduce_116 = happySpecReduce_1  73 happyReduction_116
happyReduction_116 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn71
		 ([happy_var_1]
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_3  73 happyReduction_117
happyReduction_117 (HappyAbsSyn71  happy_var_3)
	_
	(HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn71
		 (happy_var_1 : happy_var_3
	)
happyReduction_117 _ _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  74 happyReduction_118
happyReduction_118 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  74 happyReduction_119
happyReduction_119 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  74 happyReduction_120
happyReduction_120 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happyReduce 4 75 happyReduction_121
happyReduction_121 (_ `HappyStk`
	(HappyAbsSyn61  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (Function (readWord happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_122 = happySpecReduce_1  76 happyReduction_122
happyReduction_122 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  76 happyReduction_123
happyReduction_123 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happyReduce 4 77 happyReduction_124
happyReduction_124 (_ `HappyStk`
	(HappyAbsSyn42  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (Description $ readWord happy_var_3
	) `HappyStk` happyRest

happyReduce_125 = happyReduce 4 78 happyReduction_125
happyReduction_125 (_ `HappyStk`
	(HappyAbsSyn42  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (IQuote $ readWord happy_var_3
	) `HappyStk` happyRest

happyReduce_126 = happySpecReduce_1  79 happyReduction_126
happyReduction_126 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_1  79 happyReduction_127
happyReduction_127 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_127 _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1  79 happyReduction_128
happyReduction_128 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happyReduce 4 80 happyReduction_129
happyReduction_129 (_ `HappyStk`
	(HappyAbsSyn81  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (Status happy_var_3
	) `HappyStk` happyRest

happyReduce_130 = happySpecReduce_1  80 happyReduction_130
happyReduction_130 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  81 happyReduction_131
happyReduction_131 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn81
		 (readStatus happy_var_1
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happyReduce 6 82 happyReduction_132
happyReduction_132 (_ `HappyStk`
	(HappyAbsSyn61  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn58  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (InferenceInfo happy_var_1 ( readWord happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_133 = happyReduce 6 83 happyReduction_133
happyReduction_133 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn85  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (AssumptionR (L.map readWord happy_var_4)
	) `HappyStk` happyRest

happyReduce_134 = happyReduce 4 84 happyReduction_134
happyReduction_134 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (Refutation happy_var_3
	) `HappyStk` happyRest

happyReduce_135 = happySpecReduce_1  85 happyReduction_135
happyReduction_135 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn85
		 ([happy_var_1]
	)
happyReduction_135 _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_3  85 happyReduction_136
happyReduction_136 (HappyAbsSyn85  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn85
		 (happy_var_1 : happy_var_3
	)
happyReduction_136 _ _ _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_1  86 happyReduction_137
happyReduction_137 (HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn86
		 (GTerm happy_var_1
	)
happyReduction_137 _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_3  86 happyReduction_138
happyReduction_138 (HappyAbsSyn86  happy_var_3)
	_
	(HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn86
		 (ColonSep happy_var_1 happy_var_3
	)
happyReduction_138 _ _ _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_1  86 happyReduction_139
happyReduction_139 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn86
		 (GList happy_var_1
	)
happyReduction_139 _  = notHappyAtAll 

happyReduce_140 = happyReduce 4 87 happyReduction_140
happyReduction_140 (_ `HappyStk`
	(HappyAbsSyn61  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn87
		 (GApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_141 = happySpecReduce_1  87 happyReduction_141
happyReduction_141 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn87
		 (GWord happy_var_1
	)
happyReduction_141 _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_1  87 happyReduction_142
happyReduction_142 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn87
		 (GDistinctObject (stripQuotes '"' happy_var_1)
	)
happyReduction_142 _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_1  87 happyReduction_143
happyReduction_143 (HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn87
		 (happy_var_1
	)
happyReduction_143 _  = notHappyAtAll 

happyReduce_144 = happySpecReduce_1  87 happyReduction_144
happyReduction_144 (HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn87
		 (GNumber happy_var_1
	)
happyReduction_144 _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_1  87 happyReduction_145
happyReduction_145 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn87
		 (GVar happy_var_1
	)
happyReduction_145 _  = notHappyAtAll 

happyReduce_146 = happyReduce 4 88 happyReduction_146
happyReduction_146 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn87
		 (GFormulaData "$cnf" happy_var_3
	) `HappyStk` happyRest

happyReduce_147 = happyReduce 4 88 happyReduction_147
happyReduction_147 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn87
		 (GFormulaData "$fof" happy_var_3
	) `HappyStk` happyRest

happyReduce_148 = happyReduce 4 88 happyReduction_148
happyReduction_148 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn87
		 (GFormulaTerm "$fot" happy_var_3
	) `HappyStk` happyRest

happyReduce_149 = happySpecReduce_2  89 happyReduction_149
happyReduction_149 _
	_
	 =  HappyAbsSyn61
		 ([]
	)

happyReduce_150 = happySpecReduce_3  89 happyReduction_150
happyReduction_150 _
	(HappyAbsSyn61  happy_var_2)
	_
	 =  HappyAbsSyn61
		 (happy_var_2
	)
happyReduction_150 _ _ _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_1  90 happyReduction_151
happyReduction_151 (HappyAbsSyn86  happy_var_1)
	 =  HappyAbsSyn61
		 ([happy_var_1]
	)
happyReduction_151 _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_3  90 happyReduction_152
happyReduction_152 (HappyAbsSyn61  happy_var_3)
	_
	(HappyAbsSyn86  happy_var_1)
	 =  HappyAbsSyn61
		 (happy_var_1 : happy_var_3
	)
happyReduction_152 _ _ _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_1  91 happyReduction_153
happyReduction_153 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_153 _  = notHappyAtAll 

happyReduce_154 = happySpecReduce_1  91 happyReduction_154
happyReduction_154 (HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn42
		 (AtomicWord(show happy_var_1)
	)
happyReduction_154 _  = notHappyAtAll 

happyReduce_155 = happySpecReduce_1  92 happyReduction_155
happyReduction_155 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn42
		 (AtomicWord happy_var_1
	)
happyReduction_155 _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_1  92 happyReduction_156
happyReduction_156 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn42
		 (AtomicWord (stripQuotes '\'' happy_var_1)
	)
happyReduction_156 _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_1  93 happyReduction_157
happyReduction_157 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_157 _  = notHappyAtAll 

happyReduce_158 = happySpecReduce_1  94 happyReduction_158
happyReduction_158 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_158 _  = notHappyAtAll 

happyReduce_159 = happySpecReduce_1  95 happyReduction_159
happyReduction_159 (HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn95
		 (fromIntegral happy_var_1
	)
happyReduction_159 _  = notHappyAtAll 

happyReduce_160 = happySpecReduce_1  95 happyReduction_160
happyReduction_160 (HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn95
		 (happy_var_1
	)
happyReduction_160 _  = notHappyAtAll 

happyReduce_161 = happySpecReduce_1  95 happyReduction_161
happyReduction_161 (HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn95
		 (happy_var_1
	)
happyReduction_161 _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_1  96 happyReduction_162
happyReduction_162 (HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn96
		 (happy_var_1
	)
happyReduction_162 _  = notHappyAtAll 

happyReduce_163 = happySpecReduce_1  96 happyReduction_163
happyReduction_163 (HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn96
		 (happy_var_1
	)
happyReduction_163 _  = notHappyAtAll 

happyReduce_164 = happySpecReduce_3  97 happyReduction_164
happyReduction_164 (HappyAbsSyn96  happy_var_3)
	_
	(HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn95
		 (happy_var_1 % happy_var_3
	)
happyReduction_164 _ _ _  = notHappyAtAll 

happyReduce_165 = happySpecReduce_1  98 happyReduction_165
happyReduction_165 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (stripQuotes '\'' happy_var_1
	)
happyReduction_165 _  = notHappyAtAll 

happyReduce_166 = happySpecReduce_1  99 happyReduction_166
happyReduction_166 _
	 =  HappyAbsSyn48
		 ("cnf"
	)

happyReduce_167 = happySpecReduce_1  99 happyReduction_167
happyReduction_167 _
	 =  HappyAbsSyn48
		 ("$cnf"
	)

happyReduce_168 = happySpecReduce_1  99 happyReduction_168
happyReduction_168 _
	 =  HappyAbsSyn48
		 ("fof"
	)

happyReduce_169 = happySpecReduce_1  99 happyReduction_169
happyReduction_169 _
	 =  HappyAbsSyn48
		 ("include"
	)

happyReduce_170 = happySpecReduce_1  99 happyReduction_170
happyReduction_170 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_170 _  = notHappyAtAll 

happyReduce_171 = happySpecReduce_1  100 happyReduction_171
happyReduction_171 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_171 _  = notHappyAtAll 

happyReduce_172 = happySpecReduce_1  101 happyReduction_172
happyReduction_172 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_172 _  = notHappyAtAll 

happyReduce_173 = happySpecReduce_1  102 happyReduction_173
happyReduction_173 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_173 _  = notHappyAtAll 

happyReduce_174 = happySpecReduce_1  103 happyReduction_174
happyReduction_174 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_174 _  = notHappyAtAll 

happyReduce_175 = happySpecReduce_1  104 happyReduction_175
happyReduction_175 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_175 _  = notHappyAtAll 

happyReduce_176 = happySpecReduce_1  105 happyReduction_176
happyReduction_176 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_176 _  = notHappyAtAll 

happyReduce_177 = happySpecReduce_1  106 happyReduction_177
happyReduction_177 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_177 _  = notHappyAtAll 

happyReduce_178 = happySpecReduce_1  107 happyReduction_178
happyReduction_178 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_178 _  = notHappyAtAll 

happyReduce_179 = happySpecReduce_1  108 happyReduction_179
happyReduction_179 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_179 _  = notHappyAtAll 

happyReduce_180 = happySpecReduce_1  109 happyReduction_180
happyReduction_180 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_180 _  = notHappyAtAll 

happyReduce_181 = happySpecReduce_1  110 happyReduction_181
happyReduction_181 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_181 _  = notHappyAtAll 

happyReduce_182 = happySpecReduce_1  111 happyReduction_182
happyReduction_182 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_182 _  = notHappyAtAll 

happyReduce_183 = happySpecReduce_1  112 happyReduction_183
happyReduction_183 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_183 _  = notHappyAtAll 

happyReduce_184 = happySpecReduce_1  113 happyReduction_184
happyReduction_184 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_184 _  = notHappyAtAll 

happyReduce_185 = happySpecReduce_1  114 happyReduction_185
happyReduction_185 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_185 _  = notHappyAtAll 

happyReduce_186 = happySpecReduce_1  115 happyReduction_186
happyReduction_186 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_186 _  = notHappyAtAll 

happyReduce_187 = happySpecReduce_1  116 happyReduction_187
happyReduction_187 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_187 _  = notHappyAtAll 

happyReduce_188 = happySpecReduce_1  117 happyReduction_188
happyReduction_188 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_188 _  = notHappyAtAll 

happyReduce_189 = happySpecReduce_1  118 happyReduction_189
happyReduction_189 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_189 _  = notHappyAtAll 

happyReduce_190 = happySpecReduce_1  119 happyReduction_190
happyReduction_190 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_190 _  = notHappyAtAll 

happyReduce_191 = happySpecReduce_1  120 happyReduction_191
happyReduction_191 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_191 _  = notHappyAtAll 

happyReduce_192 = happySpecReduce_1  121 happyReduction_192
happyReduction_192 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_192 _  = notHappyAtAll 

happyReduce_193 = happySpecReduce_1  122 happyReduction_193
happyReduction_193 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_193 _  = notHappyAtAll 

happyReduce_194 = happySpecReduce_1  123 happyReduction_194
happyReduction_194 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_194 _  = notHappyAtAll 

happyReduce_195 = happySpecReduce_1  124 happyReduction_195
happyReduction_195 (HappyTerminal (SingleQuoted happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_195 _  = notHappyAtAll 

happyReduce_196 = happySpecReduce_1  125 happyReduction_196
happyReduction_196 (HappyTerminal (DoubleQuoted happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_196 _  = notHappyAtAll 

happyReduce_197 = happySpecReduce_1  126 happyReduction_197
happyReduction_197 (HappyTerminal (DollarWord happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_197 _  = notHappyAtAll 

happyReduce_198 = happySpecReduce_1  127 happyReduction_198
happyReduction_198 (HappyTerminal (DollarDollarWord happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_198 _  = notHappyAtAll 

happyReduce_199 = happySpecReduce_1  128 happyReduction_199
happyReduction_199 (HappyTerminal (UpperWord happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_199 _  = notHappyAtAll 

happyReduce_200 = happySpecReduce_1  129 happyReduction_200
happyReduction_200 (HappyTerminal (LowerWord happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_200 _  = notHappyAtAll 

happyReduce_201 = happySpecReduce_1  130 happyReduction_201
happyReduction_201 (HappyTerminal (SignedInt happy_var_1))
	 =  HappyAbsSyn96
		 (happy_var_1
	)
happyReduction_201 _  = notHappyAtAll 

happyReduce_202 = happySpecReduce_1  131 happyReduction_202
happyReduction_202 (HappyTerminal (UnsignedInt happy_var_1))
	 =  HappyAbsSyn96
		 (happy_var_1
	)
happyReduction_202 _  = notHappyAtAll 

happyReduce_203 = happySpecReduce_1  132 happyReduction_203
happyReduction_203 (HappyTerminal (Real happy_var_1))
	 =  HappyAbsSyn95
		 (happy_var_1
	)
happyReduction_203 _  = notHappyAtAll 

happyReduce_204 = happySpecReduce_1  133 happyReduction_204
happyReduction_204 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_204 _  = notHappyAtAll 

happyReduce_205 = happySpecReduce_1  134 happyReduction_205
happyReduction_205 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_205 _  = notHappyAtAll 

happyReduce_206 = happySpecReduce_1  135 happyReduction_206
happyReduction_206 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_206 _  = notHappyAtAll 

happyReduce_207 = happySpecReduce_1  136 happyReduction_207
happyReduction_207 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_207 _  = notHappyAtAll 

happyReduce_208 = happySpecReduce_1  137 happyReduction_208
happyReduction_208 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_208 _  = notHappyAtAll 

happyReduce_209 = happySpecReduce_1  138 happyReduction_209
happyReduction_209 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_209 _  = notHappyAtAll 

happyReduce_210 = happySpecReduce_1  139 happyReduction_210
happyReduction_210 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_210 _  = notHappyAtAll 

happyReduce_211 = happySpecReduce_1  140 happyReduction_211
happyReduction_211 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_211 _  = notHappyAtAll 

happyReduce_212 = happySpecReduce_1  141 happyReduction_212
happyReduction_212 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_212 _  = notHappyAtAll 

happyReduce_213 = happySpecReduce_1  142 happyReduction_213
happyReduction_213 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_213 _  = notHappyAtAll 

happyReduce_214 = happySpecReduce_1  143 happyReduction_214
happyReduction_214 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_214 _  = notHappyAtAll 

happyReduce_215 = happySpecReduce_1  144 happyReduction_215
happyReduction_215 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_215 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 193 193 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Oper ":" -> cont 145;
	Comma -> cont 146;
	Dot -> cont 147;
	Lbrack -> cont 148;
	LP -> cont 149;
	Rbrack -> cont 150;
	RP -> cont 151;
	Oper "=" -> cont 152;
	Oper "<=>" -> cont 153;
	Oper "<=" -> cont 154;
	Oper "=>" -> cont 155;
	Oper "~&" -> cont 156;
	Oper "!=" -> cont 157;
	Oper "~|" -> cont 158;
	Oper "<~>" -> cont 159;
	Oper "&" -> cont 160;
	Oper "!" -> cont 161;
	Oper "?" -> cont 162;
	Oper "~" -> cont 163;
	Oper "|" -> cont 164;
	LowerWord "ac" -> cont 165;
	LowerWord "assumptions" -> cont 166;
	LowerWord "cnf" -> cont 167;
	LowerWord "creator" -> cont 168;
	LowerWord "description" -> cont 169;
	LowerWord "equality" -> cont 170;
	LowerWord "file" -> cont 171;
	LowerWord "fof" -> cont 172;
	LowerWord "include" -> cont 173;
	LowerWord "inference" -> cont 174;
	LowerWord "introduced" -> cont 175;
	LowerWord "iquote" -> cont 176;
	LowerWord "refutation" -> cont 177;
	LowerWord "status" -> cont 178;
	LowerWord "theory" -> cont 179;
	DollarWord "$cnf" -> cont 180;
	DollarWord "$fof" -> cont 181;
	DollarWord "$fot" -> cont 182;
	DoubleQuoted happy_dollar_dollar -> cont 183;
	DollarDollarWord happy_dollar_dollar -> cont 184;
	DollarWord happy_dollar_dollar -> cont 185;
	LowerWord happy_dollar_dollar -> cont 186;
	Real happy_dollar_dollar -> cont 187;
	SignedInt happy_dollar_dollar -> cont 188;
	SingleQuoted happy_dollar_dollar -> cont 189;
	Slash -> cont 190;
	UnsignedInt happy_dollar_dollar -> cont 191;
	UpperWord happy_dollar_dollar -> cont 192;
	_ -> happyError' (tk:tks)
	}

happyError_ 193 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . ((\xs -> case xs of
                    xs -> error ("Parse error, pos: " ++ show (L.take 25 xs))))

parseTSTP tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


stripQuotes which (x:xs) = go xs
  where
    go [x] = []
    go ('\\':'\\':xs) = '\\':go xs
    go ('\\':which:xs) = which:go xs
    go (x:xs) = x:go xs

fApp2pApp (FunApp x args) = PredApp x args
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib64/ghc-8.0.2/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc5892_0/ghc_2.h" #-}








































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

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
happyFail  i tk (HappyState (action)) sts stk =
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

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
