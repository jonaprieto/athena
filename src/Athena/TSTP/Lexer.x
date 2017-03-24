{

-- | TSTP.Lexer module

{-# OPTIONS -fno-warn-incomplete-patterns       #-}
{-# OPTIONS -fno-warn-incomplete-uni-patterns   #-}
{-# OPTIONS -fno-warn-missing-signatures        #-}
{-# OPTIONS -fno-warn-monomorphism-restriction  #-}
{-# OPTIONS -fno-warn-name-shadowing            #-}
{-# OPTIONS -fno-warn-unused-matches            #-}

module Athena.TSTP.Lexer where

------------------------------------------------------------------------------

import Data.Ratio

------------------------------------------------------------------------------

}

%wrapper "posn"

$sign               = [\+\-]
$exponent           = [Ee]
$numeric            = 0-9
$non_zero_numeric   = 1-9
$lower_alpha        = a-z
$upper_alpha        = A-Z
$alpha_numeric      = [$lower_alpha$upper_alpha$numeric\_]
$dollar             = \$
$printable_char     = .
$viewable_char      = [$printable_char\n]


@not_star_slash = ( ([\n] | [^\*]) * ("*"+) ([\n] | [^\/\*]) )* ([\n] | [^\*])*
@sq_char        = [^\\\'] | [\\][\\\']
@do_char        = [^\\\"] | [\\][\\\"]

@metis_sep      = "--" ("-"*)

@decimal_natural  = [0]| $non_zero_numeric $numeric*
@signed_decimal   = $sign @decimal_natural
@decimal          = @signed_decimal | @decimal_natural
@dot_decimal      = "." $numeric $numeric*
@decimal_fraction = @decimal @dot_decimal
@decimal_exponent = ( @decimal | @decimal_fraction ) $exponent @decimal


tokens :-

  $white+                                      ;
  "("                                          { withPos $ const LP }
  "["                                          { withPos $ const Lbrack }
  "]"                                          { withPos $ const Rbrack }
  ")"                                          { withPos $ const RP }
  ","                                          { withPos $ const Comma }
  "!="|"="|"<=>"|"<="|"=>"|"<~>"|"&"|"|"
      |"~|"|"~&"|"!"|"?"|":"|"~"               { withPos $ Oper }
  "."                                          { withPos $ const Dot }
  ("%"|"#")$printable_char*                    { withPos $ CommentToken } -- comment line
  "/*" @not_star_slash "*"("*"*)"/"            { withPos $ CommentToken } -- comment block
  "SZS"$printable_char*                        ;
  @metis_sep                                   ;
  [\'] @sq_char* [\']                          { withPos SingleQuoted }
  [\"] @do_char* [\"]                          { withPos DoubleQuoted }
  $dollar $dollar $lower_alpha $alpha_numeric* { withPos DollarDollarWord }
  $dollar $lower_alpha $alpha_numeric*         { withPos DollarWord }
  $upper_alpha $alpha_numeric*                 { withPos UpperWord }
  $lower_alpha $alpha_numeric*                 { withPos LowerWord }
  "*"                                          { withPos $ const Star }
  "+"                                          { withPos $ const Plus }
  ">"                                          { withPos $ const Rangle }
  @decimal_fraction | @decimal_exponent        { withPos (Real . readDecimalFraction . stripPlus) }
  $sign @decimal_natural                       { withPos (SignedInt . readInteger . stripPlus) }
  @decimal_natural                             { withPos (UnsignedInt . readInteger . stripPlus) }
  "/"                                          { withPos $ const Slash }


{
-- Each action has type :: String -> Token

withPos f pos s = (pos, f s)

-- The token type:
data Token = Comma
           | CommentToken String
           | DollarDollarWord String
           | DollarWord String
           | Dot
           | DoubleQuoted String
           | Lbrack
           | LowerWord String
           | LP
           | MetisComment
           | MetisInfo String
           | Oper String
           | Plus
           | Rangle
           | Rbrack
           | Real Rational
           | RP
           | SignedInt Integer
           | SingleQuoted String
           | Slash
           | Star
           | UnsignedInt Integer
           | UpperWord String
           deriving (Eq,Ord,Show)

-- alex defines: alexScanTokens

stripPlus :: String -> String
stripPlus ('+':xs) = xs
stripPlus xs = xs

readDecimalFraction :: String -> Rational
readDecimalFraction ('-':cs) = -(readUnsignedDecimalFraction cs)
readDecimalFraction cs       = readUnsignedDecimalFraction cs

readInteger :: String -> Integer
readInteger = read

readUnsignedDecimalFraction :: String -> Rational
readUnsignedDecimalFraction cs =
    case break (=='.') cs of
         (_,"") -> case breakExponent cs of
            (cs2,_:cs2') -> readIntegerRat cs2 * readExponent cs2'

         (cs1,_:cs1') -> case breakExponent cs1' of
            (_,"") -> readIntegerRat cs1 + readFraction cs1'
            (cs2,_:cs2') -> (readIntegerRat cs1 + readFraction cs2) * readExponent cs2'
  where
    breakExponent = break (`elem` "Ee")

    readExponent :: String -> Rational
    readExponent = (10^^) . readInteger . stripPlus

    readFraction :: String -> Rational
    readFraction cs = readInteger cs % (10^(length cs))

    readIntegerRat :: String -> Rational
    readIntegerRat = fromIntegral . readInteger
}
