{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
-- | TSTP.Parser module

{-# OPTIONS -fno-warn-incomplete-patterns       #-}
{-# OPTIONS -fno-warn-incomplete-uni-patterns   #-}
{-# OPTIONS -fno-warn-missing-signatures        #-}
{-# OPTIONS -fno-warn-monomorphism-restriction  #-}
{-# OPTIONS -fno-warn-name-shadowing            #-}
{-# OPTIONS -fno-warn-unused-matches            #-}

module Athena.TSTP.Parser where

------------------------------------------------------------------------------

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
import TSTP.Base
import TSTP.Lexer

------------------------------------------------------------------------------
import qualified Data.Array as Happy_Data_Array
import qualified System.IO as Happy_System_IO
import qualified System.IO.Unsafe as Happy_System_IO_Unsafe
import qualified Debug.Trace as Happy_Debug_Trace
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
	| HappyAbsSyn145 ([String])

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,388) ([197,0,783,197,0,0,0,825,825,780,780,0,780,0,734,780,734,0,826,0,0,0,0,0,0,0,0,0,778,778,778,778,778,0,806,0,836,758,0,0,0,0,0,836,797,0,0,797,1530,1314,797,0,0,0,0,0,1213,0,0,0,794,0,0,0,0,0,0,190,0,189,0,786,0,0,0,178,0,786,43,0,786,0,0,0,0,0,742,0,1314,0,0,1314,0,0,0,0,0,0,0,731,731,731,731,731,731,731,731,731,776,0,757,0,0,1423,2170,0,765,768,0,1423,723,759,1073,0,0,0,0,0,0,0,0,0,0,759,708,2170,2170,2170,2170,0,2170,0,0,705,705,717,704,1314,0,0,0,0,0,0,1314,1314,704,704,704,704,704,704,704,745,743,678,0,0,0,0,0,0,0,725,728,0,0,710,713,0,0,0,0,0,0,0,713,707,707,707,0,0,711,0,0,0,0,0,0,0,0,706,706,706,706,706,645,645,645,645,645,682,0,673,0,0,0,635,0,0,0,0,0,265,836,115,632,265,0,670,0,0,0,2170,619,665,616,0,1314,0,1314,0,0,644,647,0,1314,613,0,0,0,2158,659,0,659,0,659,0,0,610,610,656,0,656,0,0,649,649,0,0,649,649,734,648,646,643,641,0,0,0,0,0,0,0,0,0,636,0,636,636,636,636,636,591,591,591,591,591,0,0,0,0,0,0,0,0,0,265,612,634,836,265,1392,2197,0,265,630,0,0,0,0,0,627,628,0,0,628,599,0,0,606,597,0,1173,0,42,597,597,594,594,0,734,594,594,0,0,593,595,0,0,2170,1314,1530,574,0,1392,0,1392,1392,575,265,565,561,569,553,0,0,553,0,553,553,553,734,553,0,0,0,0,0,0,0,0,549,265,545,0,0,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,388) ([64,0,0,59,0,0,0,455,454,408,407,0,406,0,2051,404,2038,0,444,0,0,0,0,0,0,0,0,0,401,397,396,394,388,0,427,0,71,385,0,0,0,0,0,52,425,0,0,424,1256,238,8,0,0,0,0,0,110,0,0,0,420,0,0,0,0,0,0,161,0,0,0,421,0,0,0,0,0,387,0,0,361,0,0,0,0,0,0,0,116,0,0,1036,0,0,0,0,0,0,0,375,358,347,346,345,344,325,324,315,1,0,23,0,0,1365,1575,0,-16,349,0,1472,308,342,1193,0,0,0,0,0,0,0,0,0,0,341,303,1794,1700,1684,1919,0,1904,0,0,296,295,55,294,922,0,0,0,0,0,0,808,694,293,292,290,287,283,282,280,316,314,271,0,0,0,0,0,0,0,34,60,0,0,312,310,0,0,0,0,0,0,0,304,306,305,288,0,0,33,0,0,0,0,0,0,0,0,278,277,276,275,239,228,211,210,201,191,224,0,-7,0,0,0,182,0,0,0,0,0,792,766,-53,-21,289,0,-19,0,0,0,1590,54,222,181,0,580,0,466,0,0,14,36,0,352,179,0,0,0,1979,221,0,-8,0,30,0,0,176,158,2,0,-22,0,0,219,217,0,0,212,203,1409,-41,202,198,157,0,0,0,0,0,0,0,0,0,200,0,199,196,195,194,167,148,119,118,117,114,0,0,0,0,0,0,0,0,0,631,-42,154,878,517,562,2012,0,403,150,0,0,0,0,0,131,130,0,0,122,-4,0,0,129,125,0,220,0,-71,123,88,113,104,0,1018,103,102,0,0,-23,83,0,0,1810,-6,1146,-24,0,448,0,676,334,97,973,-44,-52,-49,77,0,0,70,0,41,-1,-2,905,-3,0,0,0,0,0,0,0,0,87,859,-45,0,0,0,0
	])

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,388) ([-2,0,0,-2,-4,-6,-5,0,0,-217,-217,-192,-217,-193,0,-217,0,-3,0,-154,-156,-169,-167,-168,-170,-157,-171,-155,-217,-217,-217,-217,-217,-172,0,-218,0,-217,-203,-196,-201,-194,-195,0,0,-11,-176,0,0,0,-10,-12,-14,-15,-18,-17,-13,-27,-28,-32,0,-25,-52,-51,-56,-55,-53,0,-63,-54,-68,-70,-65,-72,-73,-57,-77,-79,-62,-81,-83,-64,-71,-80,-84,-75,-160,-161,0,-43,-44,0,-74,-158,-159,-85,-163,-164,-162,-217,-217,-217,-217,-217,-217,-217,-217,-217,-10,-34,-37,-41,-39,0,0,-40,0,0,-36,0,-217,0,0,-200,-202,-204,-198,-199,-197,-191,-188,-187,-33,0,0,0,0,0,0,-59,0,-60,-61,-217,-217,0,-217,0,-45,-47,-50,-49,-48,-46,0,0,-217,-217,-217,-217,-217,-217,-217,0,0,-217,-189,-181,-182,-183,-180,-184,-179,-20,-23,-16,-174,0,-30,-186,-185,-42,-66,-76,-67,-58,-86,0,0,0,-165,-26,-114,-88,-92,-89,-90,-102,-103,-104,-91,0,0,0,0,0,-217,-217,-217,-217,-217,0,-190,-37,-35,-38,-8,-217,-207,-208,-205,-206,-211,0,0,0,0,0,-9,0,-82,-78,-69,0,0,0,-217,-22,0,-19,0,-173,-7,-20,-23,-175,0,-217,-31,-87,-113,0,0,-94,-107,-166,-114,-110,-109,-217,-217,-114,-101,-114,-112,-177,0,0,-210,-209,0,0,0,0,0,0,-117,-121,-119,-123,-124,-120,-127,-131,-128,-129,0,-116,0,0,0,0,0,-217,-217,-217,-217,-217,-178,-29,-24,-21,-213,-215,-212,-216,-214,0,0,0,0,0,0,0,-115,0,0,-106,-105,-108,-100,-111,0,0,-118,-146,-152,-138,-144,-140,0,-142,-145,0,-143,-217,0,0,0,0,-132,0,0,0,-125,-135,0,-136,-130,-126,0,0,0,0,-150,0,-122,0,0,0,0,0,-95,-99,0,-153,-139,0,-151,0,0,0,0,0,-134,-137,-149,-148,-147,-141,-133,-97,0,0,0,-93,-96,-98
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,2339) ([-1,7,8,9,10,11,5,13,57,15,16,64,18,5,21,31,32,23,24,61,26,27,28,29,30,96,12,68,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,21,67,12,5,100,68,14,8,97,102,62,99,13,98,6,0,1,2,3,4,0,1,2,3,4,67,141,17,17,94,14,99,99,6,100,98,133,134,88,89,90,91,92,93,130,95,96,49,100,109,97,97,97,67,102,120,67,100,100,49,49,111,112,114,100,115,116,117,118,119,120,121,122,123,124,125,126,127,128,7,8,9,10,11,114,13,100,15,16,100,18,25,21,114,97,23,24,26,26,27,28,29,30,95,114,113,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,95,97,116,117,118,119,85,113,97,116,117,125,124,124,116,117,85,100,96,98,8,116,117,118,119,13,31,32,33,98,125,8,8,97,97,97,13,13,88,89,90,91,92,93,97,95,96,103,104,105,106,107,108,96,23,96,100,113,114,28,97,111,112,98,100,115,116,117,118,119,120,121,122,123,124,125,126,127,128,7,8,9,10,11,100,13,98,15,16,141,18,100,141,141,141,23,24,96,26,27,28,29,30,49,109,110,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,23,141,96,96,96,28,29,96,96,99,96,141,97,36,82,83,84,85,86,42,88,97,45,91,92,93,97,95,97,141,98,99,141,100,141,141,102,101,88,89,90,91,92,93,141,95,96,96,116,117,118,119,120,121,141,54,124,125,126,127,128,111,112,141,141,115,116,117,118,119,120,121,122,123,124,125,126,127,128,15,16,141,18,96,96,96,96,23,24,88,26,27,28,29,30,49,95,97,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,97,97,100,116,117,118,119,120,100,99,141,97,125,101,82,83,84,85,86,141,88,141,141,91,92,93,141,95,127,141,98,141,141,141,141,141,97,97,88,89,90,91,92,93,97,95,96,141,116,117,118,119,120,121,141,96,124,125,126,127,128,111,112,141,141,115,116,117,118,119,120,121,122,123,124,125,126,127,128,15,16,96,18,141,141,141,141,23,24,88,26,27,28,29,30,49,95,141,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,141,96,98,116,117,118,119,120,100,100,141,100,125,141,82,83,84,85,86,141,88,141,141,91,92,93,141,95,100,141,98,141,141,141,96,96,7,4,88,89,90,91,92,93,7,95,96,2,116,117,118,119,120,121,1,6,124,125,126,127,128,111,112,4,6,115,116,117,118,119,120,121,122,123,124,125,126,127,128,15,16,2,18,6,1,7,5,23,24,88,26,27,28,29,30,49,95,7,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,2,4,2,116,117,118,119,120,4,27,49,5,125,2,82,83,84,85,86,6,88,5,4,91,92,93,7,95,2,49,98,2,49,16,20,49,1,48,88,89,90,91,92,93,4,95,96,45,116,117,118,119,120,121,49,3,124,125,126,127,128,111,112,20,49,115,116,117,118,119,120,121,122,123,124,125,126,127,128,15,16,5,18,2,7,2,6,23,24,88,26,27,28,29,30,49,95,49,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,16,20,3,116,117,118,119,120,7,49,49,47,125,23,82,83,84,85,28,29,88,48,7,91,92,93,36,95,49,8,98,7,42,20,2,45,49,47,88,89,90,91,92,93,46,95,96,5,116,117,118,119,120,121,4,2,124,125,126,127,128,111,112,49,2,115,116,117,118,119,120,121,122,123,124,125,126,127,128,15,16,59,18,49,2,49,5,23,24,50,26,27,28,29,30,-1,-1,-1,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,66,23,-1,95,-1,-1,28,29,-1,-1,-1,-1,-1,-1,36,-1,-1,-1,-1,-1,42,-1,88,-1,116,117,118,119,-1,95,-1,-1,-1,125,-1,-1,-1,-1,88,89,90,91,92,93,-1,95,96,-1,-1,-1,116,117,118,119,120,-1,55,56,-1,125,-1,111,112,-1,-1,115,116,117,118,119,120,121,122,123,124,125,126,127,128,15,16,-1,18,-1,-1,-1,-1,23,24,88,26,27,28,29,30,-1,95,77,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,-1,95,-1,116,117,118,119,120,-1,-1,-1,-1,125,-1,81,-1,-1,-1,-1,-1,87,88,116,117,118,119,-1,-1,95,-1,-1,125,-1,-1,-1,-1,-1,-1,88,89,90,91,92,93,-1,95,96,-1,-1,116,117,118,119,120,-1,-1,55,56,125,-1,127,111,112,-1,-1,115,116,117,118,119,120,121,122,123,124,125,126,127,128,15,16,-1,18,-1,-1,-1,-1,23,24,88,26,27,28,29,30,-1,95,-1,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,-1,-1,-1,116,117,118,119,120,-1,-1,23,24,125,81,27,28,29,30,31,87,88,-1,35,36,-1,-1,-1,95,-1,42,-1,-1,45,-1,47,-1,-1,-1,88,89,90,91,92,93,-1,95,96,-1,116,117,118,119,120,-1,-1,-1,-1,125,-1,127,-1,111,112,-1,-1,115,116,117,118,119,120,121,122,123,124,125,126,127,128,19,20,-1,22,23,-1,-1,26,27,28,29,30,4,-1,6,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,23,-1,-1,-1,-1,28,29,-1,-1,-1,-1,-1,-1,36,37,38,39,-1,-1,42,43,44,45,-1,47,48,9,10,11,12,-1,14,15,16,-1,-1,-1,20,88,89,90,91,92,93,-1,95,96,-1,51,52,53,-1,-1,-1,-1,58,-1,60,61,-1,63,-1,65,-1,-1,115,116,117,118,119,120,121,122,123,124,125,126,127,128,19,20,-1,22,23,87,88,26,27,28,29,30,-1,95,-1,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,-1,-1,-1,116,117,118,119,120,-1,-1,-1,-1,125,5,127,-1,129,130,131,132,-1,-1,135,-1,-1,17,18,19,-1,-1,-1,23,-1,-1,-1,-1,28,29,88,89,90,91,92,93,36,95,96,39,40,41,42,43,44,45,-1,47,48,-1,-1,-1,-1,-1,-1,-1,-1,115,116,117,118,119,120,121,122,123,124,125,126,127,128,20,-1,22,23,-1,-1,26,27,28,29,30,4,-1,-1,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,23,-1,-1,-1,-1,28,29,-1,-1,-1,-1,-1,-1,36,37,38,39,-1,-1,42,43,44,45,-1,47,48,-1,19,-1,-1,-1,23,-1,-1,-1,-1,28,29,88,89,90,91,92,93,36,95,-1,39,40,41,42,43,44,45,-1,47,48,-1,-1,-1,-1,-1,-1,-1,-1,115,116,117,118,119,120,121,122,123,124,125,126,127,128,22,23,87,88,26,27,28,29,30,-1,95,-1,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,-1,-1,-1,116,117,118,119,120,-1,-1,-1,-1,125,5,127,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,19,-1,-1,-1,23,-1,-1,-1,-1,28,29,88,89,90,91,92,93,36,95,-1,39,40,41,42,43,44,45,-1,47,48,-1,-1,-1,-1,-1,-1,-1,-1,115,116,117,118,119,120,121,122,123,124,125,126,127,128,26,27,28,29,30,-1,-1,-1,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,88,89,90,91,92,93,-1,95,-1,-1,-1,-1,-1,-1,-1,88,89,90,91,92,93,-1,95,-1,-1,-1,-1,-1,116,117,118,119,120,121,122,123,124,125,126,127,128,-1,-1,116,117,118,119,120,121,122,123,124,125,126,127,128,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,88,89,90,91,92,93,-1,95,-1,-1,-1,-1,-1,-1,-1,-1,88,89,90,91,92,93,-1,95,-1,-1,-1,-1,116,117,118,119,120,121,122,123,124,125,126,127,128,-1,-1,-1,116,117,118,119,120,121,122,123,124,125,126,127,128,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,88,89,90,91,92,93,-1,95,-1,-1,-1,-1,-1,-1,-1,-1,88,89,90,91,92,93,-1,95,-1,-1,-1,-1,116,117,118,119,120,121,122,123,124,125,126,127,128,-1,-1,-1,116,117,118,119,120,121,122,123,124,125,126,127,128,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,88,89,90,91,92,93,-1,95,-1,-1,-1,-1,-1,-1,-1,88,89,90,91,92,93,-1,95,-1,-1,-1,-1,-1,116,117,118,119,120,121,122,123,124,125,126,127,128,54,-1,116,117,118,119,120,121,122,123,124,125,126,127,128,69,70,71,72,73,74,75,76,-1,78,79,80,-1,-1,-1,-1,-1,-1,54,88,-1,-1,-1,-1,-1,-1,95,-1,-1,-1,99,-1,-1,69,70,71,72,73,74,75,76,-1,78,79,80,-1,-1,116,117,118,119,120,88,-1,-1,-1,125,-1,-1,95,-1,-1,-1,-1,-1,-1,-1,136,137,138,139,140,-1,-1,-1,-1,-1,87,88,-1,116,117,118,119,120,95,-1,-1,-1,125,87,88,-1,-1,-1,-1,-1,-1,95,-1,136,137,138,139,140,-1,116,117,118,119,120,-1,-1,-1,-1,125,6,127,-1,116,117,118,119,120,-1,-1,-1,-1,125,-1,127,-1,22,23,-1,25,-1,-1,28,29,-1,-1,32,33,34,23,36,-1,-1,-1,28,29,42,-1,-1,45,-1,-1,36,-1,-1,39,40,41,42,43,44,45,-1,47,48,22,23,-1,25,-1,-1,28,29,-1,-1,32,33,34,-1,36,-1,-1,-1,-1,-1,42,-1,-1,45,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,2339) ([0,370,51,52,53,54,121,55,381,56,57,256,58,163,215,138,139,59,60,342,61,62,63,64,65,352,301,316,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,118,266,239,16,383,250,300,-67,385,382,271,384,-67,251,44,17,3,4,5,6,2,3,4,5,6,267,41,248,177,254,237,368,373,47,229,251,257,258,82,83,84,85,86,87,202,20,88,13,272,141,374,376,377,270,357,255,228,122,229,178,178,89,90,119,122,91,21,22,23,24,25,92,93,94,95,26,96,97,98,133,51,52,53,54,240,55,229,56,57,229,58,147,260,119,378,59,60,261,61,62,63,64,65,45,240,238,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,45,379,21,22,23,24,387,238,380,7,8,26,95,95,7,8,364,372,350,333,-76,21,22,23,24,-76,138,139,140,333,26,-66,144,344,345,348,-66,145,82,83,84,85,86,87,349,20,88,148,149,150,151,152,153,351,10,355,358,154,155,11,356,89,90,360,359,91,21,22,23,24,25,92,93,94,95,26,96,97,98,50,51,52,53,54,322,55,341,56,57,302,58,313,303,304,305,59,60,307,61,62,63,64,65,325,141,142,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,10,306,308,309,310,11,29,311,312,314,315,268,318,30,326,327,328,329,353,31,331,319,32,332,86,87,320,20,321,269,333,354,298,273,245,265,246,216,82,83,84,85,86,87,218,20,88,223,21,22,23,24,25,334,219,252,95,26,96,97,98,89,90,220,221,91,21,22,23,24,25,92,93,94,95,26,96,97,98,299,57,222,58,224,225,226,227,59,60,253,61,62,63,64,65,325,20,230,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,231,232,233,21,22,23,24,25,234,235,241,164,26,242,326,327,328,329,365,166,331,167,168,332,86,87,169,20,190,170,333,171,172,176,179,180,191,211,82,83,84,85,86,87,214,20,88,212,21,22,23,24,25,334,123,135,95,26,96,97,98,89,90,124,125,91,21,22,23,24,25,92,93,94,95,26,96,97,98,243,57,136,58,126,127,128,129,59,60,323,61,62,63,64,65,325,20,130,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,131,137,145,21,22,23,24,25,48,49,46,36,26,38,326,327,328,329,367,39,331,40,41,332,86,87,42,20,43,33,333,35,11,13,14,16,166,147,82,83,84,85,86,87,166,20,88,38,21,22,23,24,25,334,248,237,95,26,96,97,98,89,90,147,237,91,21,22,23,24,25,92,93,94,95,26,96,97,98,244,57,38,58,237,248,166,16,59,60,338,61,62,63,64,65,325,20,166,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,38,147,38,21,22,23,24,25,147,208,13,16,26,38,326,327,328,329,330,237,331,16,147,332,86,87,166,20,38,13,333,38,13,163,121,13,248,108,82,83,84,85,86,87,147,20,88,32,21,22,23,24,25,334,13,218,95,26,96,97,98,89,90,121,13,91,21,22,23,24,25,92,93,94,95,26,96,97,98,173,57,16,58,38,166,38,237,59,60,343,61,62,63,64,65,325,20,13,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,163,121,218,21,22,23,24,25,166,13,13,33,26,10,366,327,328,329,11,29,331,108,166,332,86,87,30,20,13,144,333,166,31,121,38,32,13,33,82,83,84,85,86,87,135,20,88,16,21,22,23,24,25,334,147,38,95,26,96,97,98,89,90,13,38,91,21,22,23,24,25,92,93,94,95,26,96,97,98,174,57,261,58,13,38,13,16,59,60,-1,61,62,63,64,65,0,0,0,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,263,10,0,262,0,0,11,29,0,0,0,0,0,0,30,0,0,0,0,0,31,0,264,0,21,22,23,24,0,20,0,0,0,26,0,0,0,0,82,83,84,85,86,87,0,20,88,0,0,0,21,22,23,24,25,0,386,362,0,26,0,89,90,0,0,91,21,22,23,24,25,92,93,94,95,26,96,97,98,175,57,0,58,0,0,0,0,59,60,363,61,62,63,64,65,0,20,339,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,0,340,0,21,22,23,24,25,0,0,0,0,26,0,375,0,0,0,0,0,347,19,21,22,23,24,0,0,20,0,0,26,0,0,0,0,0,0,82,83,84,85,86,87,0,20,88,0,0,21,22,23,24,25,0,0,361,362,26,0,27,89,90,0,0,91,21,22,23,24,25,92,93,94,95,26,96,97,98,132,57,0,58,0,0,0,0,59,60,363,61,62,63,64,65,0,20,0,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,0,0,0,21,22,23,24,25,0,0,10,207,26,346,208,11,29,209,210,347,19,0,211,30,0,0,0,20,0,31,0,0,32,0,33,0,0,0,82,83,84,85,86,87,0,20,88,0,21,22,23,24,25,0,0,0,0,26,0,27,0,89,90,0,0,91,21,22,23,24,25,92,93,94,95,26,96,97,98,369,109,0,110,111,0,0,112,62,63,64,65,147,0,237,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,10,0,0,0,0,11,29,0,0,0,0,0,0,336,337,338,103,0,0,31,106,107,32,0,33,108,157,158,159,160,0,161,162,163,0,0,0,121,82,83,84,85,86,87,0,20,113,0,192,193,194,0,0,0,0,195,0,196,197,0,198,0,199,0,0,114,21,22,23,24,25,92,93,94,95,26,96,97,98,108,109,0,110,111,200,19,112,62,63,64,65,0,20,0,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,0,0,0,21,22,23,24,25,0,0,0,0,26,16,27,0,201,202,203,204,0,0,205,0,0,100,101,102,0,0,0,10,0,0,0,0,11,29,82,83,84,85,86,87,30,20,113,103,104,105,31,106,107,32,0,33,108,0,0,0,0,0,0,0,0,114,21,22,23,24,25,92,93,94,95,26,96,97,98,117,0,110,111,0,0,112,62,63,64,65,147,0,0,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,10,0,0,0,0,11,29,0,0,0,0,0,0,336,337,338,103,0,0,31,106,107,32,0,33,108,0,102,0,0,0,10,0,0,0,0,11,29,82,83,84,85,86,87,30,20,0,103,104,105,31,106,107,32,0,33,108,0,0,0,0,0,0,0,0,114,21,22,23,24,25,92,93,94,95,26,96,97,98,213,111,317,19,112,62,63,64,65,0,20,0,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,0,0,0,21,22,23,24,25,0,0,0,0,26,16,27,0,0,0,0,0,0,0,0,0,0,0,0,102,0,0,0,10,0,0,0,0,11,29,82,83,84,85,86,87,30,20,0,103,104,105,31,106,107,32,0,33,108,0,0,0,0,0,0,0,0,114,21,22,23,24,25,92,93,94,95,26,96,97,98,115,62,63,64,65,0,0,0,66,116,68,69,70,71,72,73,74,75,76,77,78,79,80,81,186,68,182,70,71,72,73,74,183,76,77,184,79,80,81,249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,82,83,84,85,86,87,0,20,0,0,0,0,0,0,0,82,83,84,85,86,87,0,20,0,0,0,0,0,21,22,23,24,25,92,93,94,95,26,96,97,98,0,0,21,22,23,24,25,92,93,94,95,26,96,97,98,186,68,182,70,71,72,73,74,183,76,77,184,79,80,81,187,186,68,182,70,71,72,73,74,183,76,77,184,79,80,81,188,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,82,83,84,85,86,87,0,20,0,0,0,0,0,0,0,0,82,83,84,85,86,87,0,20,0,0,0,0,21,22,23,24,25,92,93,94,95,26,96,97,98,0,0,0,21,22,23,24,25,92,93,94,95,26,96,97,98,186,68,182,70,71,72,73,74,183,76,77,184,79,80,81,189,371,68,182,70,71,72,73,74,183,76,77,184,79,80,81,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,82,83,84,85,86,87,0,20,0,0,0,0,0,0,0,0,82,83,84,85,86,87,0,20,0,0,0,0,21,22,23,24,25,92,93,94,95,26,96,97,98,0,0,0,21,22,23,24,25,92,93,94,95,26,96,97,98,181,68,182,70,71,72,73,74,183,76,77,184,79,80,81,185,68,182,70,71,72,73,74,183,76,77,184,79,80,81,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,82,83,84,85,86,87,0,20,0,0,0,0,0,0,0,82,83,84,85,86,87,0,20,0,0,0,0,0,21,22,23,24,25,92,93,94,95,26,96,97,98,274,0,21,22,23,24,25,92,93,94,95,26,96,97,98,275,276,277,278,279,280,281,282,0,283,284,285,0,0,0,0,0,0,274,286,0,0,0,0,0,0,20,0,0,0,287,0,0,324,276,277,278,279,280,281,282,0,283,284,285,0,0,21,22,23,24,25,286,0,0,0,26,0,0,20,0,0,0,0,0,0,0,288,289,290,291,292,0,0,0,0,0,18,19,0,21,22,23,24,25,20,0,0,0,26,34,19,0,0,0,0,0,0,20,0,288,289,290,291,292,0,21,22,23,24,25,0,0,0,0,26,237,27,0,21,22,23,24,25,0,0,0,0,26,0,27,0,294,10,0,295,0,0,11,29,0,0,296,297,298,10,30,0,0,0,11,29,31,0,0,32,0,0,30,0,0,103,104,105,31,106,107,32,0,33,108,294,10,0,295,0,0,11,29,0,0,296,297,298,0,30,0,0,0,0,0,31,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (1, 217) [
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
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157),
	(158 , happyReduce_158),
	(159 , happyReduce_159),
	(160 , happyReduce_160),
	(161 , happyReduce_161),
	(162 , happyReduce_162),
	(163 , happyReduce_163),
	(164 , happyReduce_164),
	(165 , happyReduce_165),
	(166 , happyReduce_166),
	(167 , happyReduce_167),
	(168 , happyReduce_168),
	(169 , happyReduce_169),
	(170 , happyReduce_170),
	(171 , happyReduce_171),
	(172 , happyReduce_172),
	(173 , happyReduce_173),
	(174 , happyReduce_174),
	(175 , happyReduce_175),
	(176 , happyReduce_176),
	(177 , happyReduce_177),
	(178 , happyReduce_178),
	(179 , happyReduce_179),
	(180 , happyReduce_180),
	(181 , happyReduce_181),
	(182 , happyReduce_182),
	(183 , happyReduce_183),
	(184 , happyReduce_184),
	(185 , happyReduce_185),
	(186 , happyReduce_186),
	(187 , happyReduce_187),
	(188 , happyReduce_188),
	(189 , happyReduce_189),
	(190 , happyReduce_190),
	(191 , happyReduce_191),
	(192 , happyReduce_192),
	(193 , happyReduce_193),
	(194 , happyReduce_194),
	(195 , happyReduce_195),
	(196 , happyReduce_196),
	(197 , happyReduce_197),
	(198 , happyReduce_198),
	(199 , happyReduce_199),
	(200 , happyReduce_200),
	(201 , happyReduce_201),
	(202 , happyReduce_202),
	(203 , happyReduce_203),
	(204 , happyReduce_204),
	(205 , happyReduce_205),
	(206 , happyReduce_206),
	(207 , happyReduce_207),
	(208 , happyReduce_208),
	(209 , happyReduce_209),
	(210 , happyReduce_210),
	(211 , happyReduce_211),
	(212 , happyReduce_212),
	(213 , happyReduce_213),
	(214 , happyReduce_214),
	(215 , happyReduce_215),
	(216 , happyReduce_216),
	(217 , happyReduce_217)
	]

happy_n_terms = 51 :: Int
happy_n_nonterms = 142 :: Int

happyReduce_1 = happySpecReduce_0  0 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2  0 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  1 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  2 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  2 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 10 3 happyReduction_6
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

happyReduce_7 = happyReduce 10 4 happyReduction_7
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
		 (F { formula = univquantFreeVars happy_var_7
                  , name    = readWord happy_var_3
                  , role    = happy_var_5
                  , source  = happy_var_8
                  }
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  5 happyReduction_9
happyReduction_9  =  HappyAbsSyn9
		 (NoSource
	)

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn10
		 (readRole happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BinOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 11 happyReduction_18
happyReduction_18 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (L.foldl (binOp (:|:)) (BinOp happy_var_1 (:|:) happy_var_3) happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_0  12 happyReduction_19
happyReduction_19  =  HappyAbsSyn16
		 ([]
	)

happyReduce_20 = happySpecReduce_3  12 happyReduction_20
happyReduction_20 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 13 happyReduction_21
happyReduction_21 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (L.foldl (binOp (:&:)) (BinOp happy_var_1 (:&:) happy_var_3) happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_0  14 happyReduction_22
happyReduction_22  =  HappyAbsSyn16
		 ([]
	)

happyReduce_23 = happySpecReduce_3  14 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  15 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  15 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  15 happyReduction_27
happyReduction_27 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happyReduce 6 16 happyReduction_28
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

happyReduce_29 = happySpecReduce_1  17 happyReduction_29
happyReduction_29 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  17 happyReduction_30
happyReduction_30 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 : happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  18 happyReduction_31
happyReduction_31 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  18 happyReduction_32
happyReduction_32 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 ((:~:) happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  19 happyReduction_33
happyReduction_33 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  19 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  20 happyReduction_35
happyReduction_35 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (L.foldl (binOp (:|:)) happy_var_1 happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  21 happyReduction_36
happyReduction_36  =  HappyAbsSyn16
		 ([]
	)

happyReduce_37 = happySpecReduce_3  21 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  22 happyReduction_38
happyReduction_38 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  22 happyReduction_39
happyReduction_39 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 ((:~:) happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  22 happyReduction_40
happyReduction_40 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  23 happyReduction_41
happyReduction_41 (HappyAbsSyn39  happy_var_3)
	(HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn11
		 (InfixPred happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  24 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn28
		 (All
	)

happyReduce_43 = happySpecReduce_1  24 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn28
		 (Exists
	)

happyReduce_44 = happySpecReduce_1  25 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn29
		 ((:<=>:)
	)

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn29
		 ((:<=:)
	)

happyReduce_46 = happySpecReduce_1  25 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn29
		 ((:=>:)
	)

happyReduce_47 = happySpecReduce_1  25 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn29
		 ((:~&:)
	)

happyReduce_48 = happySpecReduce_1  25 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn29
		 ((:~|:)
	)

happyReduce_49 = happySpecReduce_1  25 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn29
		 ((:<~>:)
	)

happyReduce_50 = happySpecReduce_1  26 happyReduction_50
happyReduction_50 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  26 happyReduction_51
happyReduction_51 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  26 happyReduction_52
happyReduction_52 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  27 happyReduction_53
happyReduction_53 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn11
		 (fApp2pApp happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  28 happyReduction_54
happyReduction_54 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  28 happyReduction_55
happyReduction_55 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  29 happyReduction_56
happyReduction_56 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn11
		 (fApp2pApp happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  30 happyReduction_57
happyReduction_57 (HappyAbsSyn39  happy_var_3)
	(HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn11
		 (InfixPred happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  31 happyReduction_58
happyReduction_58 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  32 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn35
		 ((:=:)
	)

happyReduce_60 = happySpecReduce_1  33 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn35
		 ((:!=:)
	)

happyReduce_61 = happySpecReduce_1  34 happyReduction_61
happyReduction_61 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn11
		 (fApp2pApp happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  35 happyReduction_62
happyReduction_62 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  35 happyReduction_63
happyReduction_63 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn39
		 (Var happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  36 happyReduction_64
happyReduction_64 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  36 happyReduction_65
happyReduction_65 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  36 happyReduction_66
happyReduction_66 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  37 happyReduction_67
happyReduction_67 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn39
		 (FunApp happy_var_1 []
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happyReduce 4 37 happyReduction_68
happyReduction_68 (_ `HappyStk`
	(HappyAbsSyn54  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (FunApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_1  38 happyReduction_69
happyReduction_69 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  39 happyReduction_70
happyReduction_70 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  40 happyReduction_71
happyReduction_71 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  40 happyReduction_72
happyReduction_72 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  41 happyReduction_73
happyReduction_73 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn39
		 (DistinctObjectTerm (stripQuotes '"' happy_var_1)
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  41 happyReduction_74
happyReduction_74 (HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn39
		 (NumberLitTerm happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  42 happyReduction_75
happyReduction_75 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  43 happyReduction_76
happyReduction_76 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn39
		 (FunApp (AtomicWord happy_var_1) []
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happyReduce 4 43 happyReduction_77
happyReduction_77 (_ `HappyStk`
	(HappyAbsSyn54  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (FunApp (AtomicWord happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_78 = happySpecReduce_1  44 happyReduction_78
happyReduction_78 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  45 happyReduction_79
happyReduction_79 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  46 happyReduction_80
happyReduction_80 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn39
		 (FunApp (AtomicWord happy_var_1) []
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happyReduce 4 46 happyReduction_81
happyReduction_81 (_ `HappyStk`
	(HappyAbsSyn54  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (FunApp (AtomicWord happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_82 = happySpecReduce_1  47 happyReduction_82
happyReduction_82 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  48 happyReduction_83
happyReduction_83 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  49 happyReduction_84
happyReduction_84 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn53
		 (V happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  50 happyReduction_85
happyReduction_85 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn54
		 ([happy_var_1]
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  50 happyReduction_86
happyReduction_86 (HappyAbsSyn54  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_1 : happy_var_3
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  51 happyReduction_87
happyReduction_87 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  51 happyReduction_88
happyReduction_88 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  51 happyReduction_89
happyReduction_89 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  52 happyReduction_90
happyReduction_90 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn9
		 (Name (readWord happy_var_1)
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  52 happyReduction_91
happyReduction_91 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happyReduce 10 53 happyReduction_92
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

happyReduce_93 = happySpecReduce_1  54 happyReduction_93
happyReduction_93 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn58
		 (readRule $ readWord happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  55 happyReduction_94
happyReduction_94 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn59
		 ([happy_var_1]
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  55 happyReduction_95
happyReduction_95 (HappyAbsSyn59  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn59
		 (happy_var_1 : happy_var_3
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_2  56 happyReduction_96
happyReduction_96 (HappyAbsSyn61  happy_var_2)
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn60
		 (Parent (readWord happy_var_1) happy_var_2
	)
happyReduction_96 _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_2  57 happyReduction_97
happyReduction_97 (HappyAbsSyn61  happy_var_2)
	_
	 =  HappyAbsSyn61
		 (happy_var_2
	)
happyReduction_97 _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_0  57 happyReduction_98
happyReduction_98  =  HappyAbsSyn61
		 ([]
	)

happyReduce_99 = happyReduce 5 58 happyReduction_99
happyReduction_99 (_ `HappyStk`
	(HappyAbsSyn71  happy_var_4) `HappyStk`
	(HappyAbsSyn63  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Introduced happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_100 = happySpecReduce_1  59 happyReduction_100
happyReduction_100 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn63
		 (readType happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  60 happyReduction_101
happyReduction_101 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  60 happyReduction_102
happyReduction_102 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  60 happyReduction_103
happyReduction_103 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happyReduce 5 61 happyReduction_104
happyReduction_104 (_ `HappyStk`
	(HappyAbsSyn66  happy_var_4) `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (File    happy_var_3        happy_var_4
	) `HappyStk` happyRest

happyReduce_105 = happySpecReduce_2  62 happyReduction_105
happyReduction_105 (HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn66
		 (Just (readWord happy_var_2)
	)
happyReduction_105 _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_0  62 happyReduction_106
happyReduction_106  =  HappyAbsSyn66
		 (Nothing
	)

happyReduce_107 = happyReduce 5 63 happyReduction_107
happyReduction_107 (_ `HappyStk`
	(HappyAbsSyn71  happy_var_4) `HappyStk`
	(HappyAbsSyn68  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Theory    happy_var_3          happy_var_4
	) `HappyStk` happyRest

happyReduce_108 = happySpecReduce_1  64 happyReduction_108
happyReduction_108 _
	 =  HappyAbsSyn68
		 (Equality
	)

happyReduce_109 = happySpecReduce_1  64 happyReduction_109
happyReduction_109 _
	 =  HappyAbsSyn68
		 (AC
	)

happyReduce_110 = happyReduce 5 65 happyReduction_110
happyReduction_110 (_ `HappyStk`
	(HappyAbsSyn71  happy_var_4) `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Creator happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_111 = happySpecReduce_1  66 happyReduction_111
happyReduction_111 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn48
		 (readWord happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_2  67 happyReduction_112
happyReduction_112 (HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn71
		 (happy_var_2
	)
happyReduction_112 _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_0  67 happyReduction_113
happyReduction_113  =  HappyAbsSyn71
		 ([]
	)

happyReduce_114 = happySpecReduce_3  68 happyReduction_114
happyReduction_114 _
	(HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn71
		 (happy_var_2
	)
happyReduction_114 _ _ _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_2  68 happyReduction_115
happyReduction_115 _
	_
	 =  HappyAbsSyn71
		 ([]
	)

happyReduce_116 = happySpecReduce_1  69 happyReduction_116
happyReduction_116 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn71
		 ([happy_var_1]
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_3  69 happyReduction_117
happyReduction_117 (HappyAbsSyn71  happy_var_3)
	_
	(HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn71
		 (happy_var_1 : happy_var_3
	)
happyReduction_117 _ _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  70 happyReduction_118
happyReduction_118 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  70 happyReduction_119
happyReduction_119 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  70 happyReduction_120
happyReduction_120 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happyReduce 4 71 happyReduction_121
happyReduction_121 (_ `HappyStk`
	(HappyAbsSyn61  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (Function (readWord happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_122 = happySpecReduce_1  72 happyReduction_122
happyReduction_122 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  72 happyReduction_123
happyReduction_123 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happyReduce 4 73 happyReduction_124
happyReduction_124 (_ `HappyStk`
	(HappyAbsSyn42  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (Description $ readWord happy_var_3
	) `HappyStk` happyRest

happyReduce_125 = happyReduce 4 74 happyReduction_125
happyReduction_125 (_ `HappyStk`
	(HappyAbsSyn42  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (IQuote $ readWord happy_var_3
	) `HappyStk` happyRest

happyReduce_126 = happySpecReduce_1  75 happyReduction_126
happyReduction_126 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_1  75 happyReduction_127
happyReduction_127 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_127 _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1  75 happyReduction_128
happyReduction_128 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happyReduce 4 76 happyReduction_129
happyReduction_129 (_ `HappyStk`
	(HappyAbsSyn81  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (Status happy_var_3
	) `HappyStk` happyRest

happyReduce_130 = happySpecReduce_1  76 happyReduction_130
happyReduction_130 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  77 happyReduction_131
happyReduction_131 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn81
		 (readStatus happy_var_1
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happyReduce 6 78 happyReduction_132
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

happyReduce_133 = happyReduce 6 79 happyReduction_133
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

happyReduce_134 = happyReduce 4 80 happyReduction_134
happyReduction_134 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (Refutation happy_var_3
	) `HappyStk` happyRest

happyReduce_135 = happySpecReduce_1  81 happyReduction_135
happyReduction_135 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn85
		 ([happy_var_1]
	)
happyReduction_135 _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_3  81 happyReduction_136
happyReduction_136 (HappyAbsSyn85  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn85
		 (happy_var_1 : happy_var_3
	)
happyReduction_136 _ _ _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_1  82 happyReduction_137
happyReduction_137 (HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn86
		 (GTerm happy_var_1
	)
happyReduction_137 _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_3  82 happyReduction_138
happyReduction_138 (HappyAbsSyn86  happy_var_3)
	_
	(HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn86
		 (ColonSep happy_var_1 happy_var_3
	)
happyReduction_138 _ _ _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_1  82 happyReduction_139
happyReduction_139 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn86
		 (GList happy_var_1
	)
happyReduction_139 _  = notHappyAtAll 

happyReduce_140 = happyReduce 4 83 happyReduction_140
happyReduction_140 (_ `HappyStk`
	(HappyAbsSyn61  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn87
		 (GApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_141 = happySpecReduce_1  83 happyReduction_141
happyReduction_141 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn87
		 (GWord happy_var_1
	)
happyReduction_141 _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_1  83 happyReduction_142
happyReduction_142 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn87
		 (GDistinctObject (stripQuotes '"' happy_var_1)
	)
happyReduction_142 _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_1  83 happyReduction_143
happyReduction_143 (HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn87
		 (happy_var_1
	)
happyReduction_143 _  = notHappyAtAll 

happyReduce_144 = happySpecReduce_1  83 happyReduction_144
happyReduction_144 (HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn87
		 (GNumber happy_var_1
	)
happyReduction_144 _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_1  83 happyReduction_145
happyReduction_145 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn87
		 (GVar happy_var_1
	)
happyReduction_145 _  = notHappyAtAll 

happyReduce_146 = happyReduce 4 84 happyReduction_146
happyReduction_146 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn87
		 (GFormulaData "$cnf" happy_var_3
	) `HappyStk` happyRest

happyReduce_147 = happyReduce 4 84 happyReduction_147
happyReduction_147 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn87
		 (GFormulaData "$fof" happy_var_3
	) `HappyStk` happyRest

happyReduce_148 = happyReduce 4 84 happyReduction_148
happyReduction_148 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn87
		 (GFormulaTerm "$fot" happy_var_3
	) `HappyStk` happyRest

happyReduce_149 = happySpecReduce_2  85 happyReduction_149
happyReduction_149 _
	_
	 =  HappyAbsSyn61
		 ([]
	)

happyReduce_150 = happySpecReduce_3  85 happyReduction_150
happyReduction_150 _
	(HappyAbsSyn61  happy_var_2)
	_
	 =  HappyAbsSyn61
		 (happy_var_2
	)
happyReduction_150 _ _ _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_1  86 happyReduction_151
happyReduction_151 (HappyAbsSyn86  happy_var_1)
	 =  HappyAbsSyn61
		 ([happy_var_1]
	)
happyReduction_151 _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_3  86 happyReduction_152
happyReduction_152 (HappyAbsSyn61  happy_var_3)
	_
	(HappyAbsSyn86  happy_var_1)
	 =  HappyAbsSyn61
		 (happy_var_1 : happy_var_3
	)
happyReduction_152 _ _ _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_1  87 happyReduction_153
happyReduction_153 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_153 _  = notHappyAtAll 

happyReduce_154 = happySpecReduce_1  87 happyReduction_154
happyReduction_154 (HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn42
		 (AtomicWord(show happy_var_1)
	)
happyReduction_154 _  = notHappyAtAll 

happyReduce_155 = happySpecReduce_1  88 happyReduction_155
happyReduction_155 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn42
		 (AtomicWord happy_var_1
	)
happyReduction_155 _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_1  88 happyReduction_156
happyReduction_156 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn42
		 (AtomicWord (stripQuotes '\'' happy_var_1)
	)
happyReduction_156 _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_1  89 happyReduction_157
happyReduction_157 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_157 _  = notHappyAtAll 

happyReduce_158 = happySpecReduce_1  90 happyReduction_158
happyReduction_158 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_158 _  = notHappyAtAll 

happyReduce_159 = happySpecReduce_1  91 happyReduction_159
happyReduction_159 (HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn95
		 (fromIntegral happy_var_1
	)
happyReduction_159 _  = notHappyAtAll 

happyReduce_160 = happySpecReduce_1  91 happyReduction_160
happyReduction_160 (HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn95
		 (happy_var_1
	)
happyReduction_160 _  = notHappyAtAll 

happyReduce_161 = happySpecReduce_1  91 happyReduction_161
happyReduction_161 (HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn95
		 (happy_var_1
	)
happyReduction_161 _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_1  92 happyReduction_162
happyReduction_162 (HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn96
		 (happy_var_1
	)
happyReduction_162 _  = notHappyAtAll 

happyReduce_163 = happySpecReduce_1  92 happyReduction_163
happyReduction_163 (HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn96
		 (happy_var_1
	)
happyReduction_163 _  = notHappyAtAll 

happyReduce_164 = happySpecReduce_3  93 happyReduction_164
happyReduction_164 (HappyAbsSyn96  happy_var_3)
	_
	(HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn95
		 (happy_var_1 % happy_var_3
	)
happyReduction_164 _ _ _  = notHappyAtAll 

happyReduce_165 = happySpecReduce_1  94 happyReduction_165
happyReduction_165 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (stripQuotes '\'' happy_var_1
	)
happyReduction_165 _  = notHappyAtAll 

happyReduce_166 = happySpecReduce_1  95 happyReduction_166
happyReduction_166 _
	 =  HappyAbsSyn48
		 ("cnf"
	)

happyReduce_167 = happySpecReduce_1  95 happyReduction_167
happyReduction_167 _
	 =  HappyAbsSyn48
		 ("$cnf"
	)

happyReduce_168 = happySpecReduce_1  95 happyReduction_168
happyReduction_168 _
	 =  HappyAbsSyn48
		 ("fof"
	)

happyReduce_169 = happySpecReduce_1  95 happyReduction_169
happyReduction_169 _
	 =  HappyAbsSyn48
		 ("include"
	)

happyReduce_170 = happySpecReduce_1  95 happyReduction_170
happyReduction_170 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_170 _  = notHappyAtAll 

happyReduce_171 = happySpecReduce_2  96 happyReduction_171
happyReduction_171 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_171 _ _  = notHappyAtAll 

happyReduce_172 = happySpecReduce_2  97 happyReduction_172
happyReduction_172 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_172 _ _  = notHappyAtAll 

happyReduce_173 = happySpecReduce_2  98 happyReduction_173
happyReduction_173 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_173 _ _  = notHappyAtAll 

happyReduce_174 = happySpecReduce_2  99 happyReduction_174
happyReduction_174 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_174 _ _  = notHappyAtAll 

happyReduce_175 = happySpecReduce_2  100 happyReduction_175
happyReduction_175 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_175 _ _  = notHappyAtAll 

happyReduce_176 = happySpecReduce_2  101 happyReduction_176
happyReduction_176 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_176 _ _  = notHappyAtAll 

happyReduce_177 = happySpecReduce_2  102 happyReduction_177
happyReduction_177 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_177 _ _  = notHappyAtAll 

happyReduce_178 = happySpecReduce_2  103 happyReduction_178
happyReduction_178 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_178 _ _  = notHappyAtAll 

happyReduce_179 = happySpecReduce_2  104 happyReduction_179
happyReduction_179 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_179 _ _  = notHappyAtAll 

happyReduce_180 = happySpecReduce_2  105 happyReduction_180
happyReduction_180 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_180 _ _  = notHappyAtAll 

happyReduce_181 = happySpecReduce_2  106 happyReduction_181
happyReduction_181 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_181 _ _  = notHappyAtAll 

happyReduce_182 = happySpecReduce_2  107 happyReduction_182
happyReduction_182 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_182 _ _  = notHappyAtAll 

happyReduce_183 = happySpecReduce_2  108 happyReduction_183
happyReduction_183 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_183 _ _  = notHappyAtAll 

happyReduce_184 = happySpecReduce_2  109 happyReduction_184
happyReduction_184 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_184 _ _  = notHappyAtAll 

happyReduce_185 = happySpecReduce_2  110 happyReduction_185
happyReduction_185 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_185 _ _  = notHappyAtAll 

happyReduce_186 = happySpecReduce_2  111 happyReduction_186
happyReduction_186 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_186 _ _  = notHappyAtAll 

happyReduce_187 = happySpecReduce_2  112 happyReduction_187
happyReduction_187 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_187 _ _  = notHappyAtAll 

happyReduce_188 = happySpecReduce_2  113 happyReduction_188
happyReduction_188 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_188 _ _  = notHappyAtAll 

happyReduce_189 = happySpecReduce_2  114 happyReduction_189
happyReduction_189 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_189 _ _  = notHappyAtAll 

happyReduce_190 = happySpecReduce_2  115 happyReduction_190
happyReduction_190 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_190 _ _  = notHappyAtAll 

happyReduce_191 = happySpecReduce_2  116 happyReduction_191
happyReduction_191 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_191 _ _  = notHappyAtAll 

happyReduce_192 = happySpecReduce_2  117 happyReduction_192
happyReduction_192 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_192 _ _  = notHappyAtAll 

happyReduce_193 = happySpecReduce_2  118 happyReduction_193
happyReduction_193 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_193 _ _  = notHappyAtAll 

happyReduce_194 = happySpecReduce_2  119 happyReduction_194
happyReduction_194 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_194 _ _  = notHappyAtAll 

happyReduce_195 = happySpecReduce_2  120 happyReduction_195
happyReduction_195 _
	(HappyTerminal (SingleQuoted happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_195 _ _  = notHappyAtAll 

happyReduce_196 = happySpecReduce_2  121 happyReduction_196
happyReduction_196 _
	(HappyTerminal (DoubleQuoted happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_196 _ _  = notHappyAtAll 

happyReduce_197 = happySpecReduce_2  122 happyReduction_197
happyReduction_197 _
	(HappyTerminal (DollarWord happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_197 _ _  = notHappyAtAll 

happyReduce_198 = happySpecReduce_2  123 happyReduction_198
happyReduction_198 _
	(HappyTerminal (DollarDollarWord happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_198 _ _  = notHappyAtAll 

happyReduce_199 = happySpecReduce_2  124 happyReduction_199
happyReduction_199 _
	(HappyTerminal (UpperWord happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_199 _ _  = notHappyAtAll 

happyReduce_200 = happySpecReduce_2  125 happyReduction_200
happyReduction_200 _
	(HappyTerminal (LowerWord happy_var_1))
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_200 _ _  = notHappyAtAll 

happyReduce_201 = happySpecReduce_2  126 happyReduction_201
happyReduction_201 _
	(HappyTerminal (SignedInt happy_var_1))
	 =  HappyAbsSyn96
		 (happy_var_1
	)
happyReduction_201 _ _  = notHappyAtAll 

happyReduce_202 = happySpecReduce_2  127 happyReduction_202
happyReduction_202 _
	(HappyTerminal (UnsignedInt happy_var_1))
	 =  HappyAbsSyn96
		 (happy_var_1
	)
happyReduction_202 _ _  = notHappyAtAll 

happyReduce_203 = happySpecReduce_2  128 happyReduction_203
happyReduction_203 _
	(HappyTerminal (Real happy_var_1))
	 =  HappyAbsSyn95
		 (happy_var_1
	)
happyReduction_203 _ _  = notHappyAtAll 

happyReduce_204 = happySpecReduce_2  129 happyReduction_204
happyReduction_204 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_204 _ _  = notHappyAtAll 

happyReduce_205 = happySpecReduce_2  130 happyReduction_205
happyReduction_205 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_205 _ _  = notHappyAtAll 

happyReduce_206 = happySpecReduce_2  131 happyReduction_206
happyReduction_206 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_206 _ _  = notHappyAtAll 

happyReduce_207 = happySpecReduce_2  132 happyReduction_207
happyReduction_207 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_207 _ _  = notHappyAtAll 

happyReduce_208 = happySpecReduce_2  133 happyReduction_208
happyReduction_208 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_208 _ _  = notHappyAtAll 

happyReduce_209 = happySpecReduce_2  134 happyReduction_209
happyReduction_209 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_209 _ _  = notHappyAtAll 

happyReduce_210 = happySpecReduce_2  135 happyReduction_210
happyReduction_210 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_210 _ _  = notHappyAtAll 

happyReduce_211 = happySpecReduce_2  136 happyReduction_211
happyReduction_211 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_211 _ _  = notHappyAtAll 

happyReduce_212 = happySpecReduce_2  137 happyReduction_212
happyReduction_212 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_212 _ _  = notHappyAtAll 

happyReduce_213 = happySpecReduce_2  138 happyReduction_213
happyReduction_213 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_213 _ _  = notHappyAtAll 

happyReduce_214 = happySpecReduce_2  139 happyReduction_214
happyReduction_214 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_214 _ _  = notHappyAtAll 

happyReduce_215 = happySpecReduce_2  140 happyReduction_215
happyReduction_215 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_1
	)
happyReduction_215 _ _  = notHappyAtAll 

happyReduce_216 = happySpecReduce_0  141 happyReduction_216
happyReduction_216  =  HappyAbsSyn145
		 ([]
	)

happyReduce_217 = happySpecReduce_2  141 happyReduction_217
happyReduction_217 (HappyAbsSyn145  happy_var_2)
	(HappyTerminal (CommentToken happy_var_1))
	 =  HappyAbsSyn145
		 (happy_var_1 : happy_var_2
	)
happyReduction_217 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction 50 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	Oper ":" -> cont 1;
	Comma -> cont 2;
	Dot -> cont 3;
	Lbrack -> cont 4;
	LP -> cont 5;
	Rbrack -> cont 6;
	RP -> cont 7;
	Oper "=" -> cont 8;
	Oper "<=>" -> cont 9;
	Oper "<=" -> cont 10;
	Oper "=>" -> cont 11;
	Oper "~&" -> cont 12;
	Oper "!=" -> cont 13;
	Oper "~|" -> cont 14;
	Oper "<~>" -> cont 15;
	Oper "&" -> cont 16;
	Oper "!" -> cont 17;
	Oper "?" -> cont 18;
	Oper "~" -> cont 19;
	Oper "|" -> cont 20;
	LowerWord "ac" -> cont 21;
	LowerWord "assumptions" -> cont 22;
	LowerWord "cnf" -> cont 23;
	LowerWord "creator" -> cont 24;
	LowerWord "description" -> cont 25;
	LowerWord "equality" -> cont 26;
	LowerWord "file" -> cont 27;
	LowerWord "fof" -> cont 28;
	LowerWord "include" -> cont 29;
	LowerWord "inference" -> cont 30;
	LowerWord "introduced" -> cont 31;
	LowerWord "iquote" -> cont 32;
	LowerWord "refutation" -> cont 33;
	LowerWord "status" -> cont 34;
	LowerWord "theory" -> cont 35;
	DollarWord "$cnf" -> cont 36;
	DollarWord "$fof" -> cont 37;
	DollarWord "$fot" -> cont 38;
	DoubleQuoted happy_dollar_dollar -> cont 39;
	DollarDollarWord happy_dollar_dollar -> cont 40;
	DollarWord happy_dollar_dollar -> cont 41;
	LowerWord happy_dollar_dollar -> cont 42;
	Real happy_dollar_dollar -> cont 43;
	SignedInt happy_dollar_dollar -> cont 44;
	SingleQuoted happy_dollar_dollar -> cont 45;
	Slash -> cont 46;
	UnsignedInt happy_dollar_dollar -> cont 47;
	UpperWord happy_dollar_dollar -> cont 48;
	CommentToken happy_dollar_dollar -> cont 49;
	_ -> happyError' (tk:tks)
	}

happyError_ 50 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

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
{-# LINE 18 "<built-in>" #-}
{-# LINE 1 "/usr/local/Cellar/ghc/8.0.1_3/lib/ghc-8.0.1.20161117/include/ghcversion.h" #-}



















{-# LINE 19 "<built-in>" #-}
{-# LINE 1 "/var/folders/_s/9079w4h9665fb3q9r0s6k3b00000gn/T/ghc12761_0/ghc_2.h" #-}







































































































































































































































































































































































































{-# LINE 20 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 


{-# LINE 13 "templates/GenericTemplate.hs" #-}


{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int Happy_IntList






{-# LINE 67 "templates/GenericTemplate.hs" #-}


{-# LINE 77 "templates/GenericTemplate.hs" #-}



happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr




infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (0), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (0) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = (happyTrace ("state: " ++ show (st) ++ 
                      
{-# LINE 112 "templates/GenericTemplate.hs" #-}
          ",\ttoken: " ++ show (i) ++
                      
{-# LINE 112 "templates/GenericTemplate.hs" #-}
          ",\taction: ")) $
          

          case action of
                (0)           -> (happyTrace ("fail.\n")) $
                                     happyFail i tk st
                (-1)          -> (happyTrace ("accept.\n")) $
                                     happyAccept i tk st
                n | (n < ((0) :: Int)) -> (happyTrace ("reduce (rule " ++ show rule
                                                               
{-# LINE 120 "templates/GenericTemplate.hs" #-}
                                                   ++ ")")) $
                                                   
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = ((negate ((n + ((1) :: Int)))))
                n                 -> (happyTrace ("shift, enter state "
                                                 
{-# LINE 124 "templates/GenericTemplate.hs" #-}
                                     ++ show (new_state)
                                                 
{-# LINE 124 "templates/GenericTemplate.hs" #-}
                                     ++ "\n")) $
                                     

                                     happyShift new_state i tk st
                                     where new_state = (n - ((1) :: Int))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off + i)
         check  = if (off_i >= ((0) :: Int))
                  then (indexShortOffAddr happyCheck off_i ==  i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off








-----------------------------------------------------------------------------
-- HappyState data type (not arrays)


{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (0) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off + nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   (happyTrace (", goto state " ++ show (new_state) ++ "\n")) $
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off + nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery ((0) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (0) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (0) tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction (0) tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction (0) tk action sts ( (HappyErrorToken (i)) `HappyStk` stk)

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

