{

-- | TSTP.Parser module

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

}

%name parseTSTP
%tokentype { Token }
%error {

          ((\xs -> case xs of
                    xs -> error ("Parse error, pos: " ++ show (L.take 25 xs))))
       }

%token
 tok_colon              { Oper ":" }
 tok_comma              { Comma }
 tok_dot                { Dot }
 tok_lbra               { Lbrack }
 tok_lp                 { LP }
 tok_rbra               { Rbrack }
 tok_rp                 { RP }

 tok_equals             { Oper "=" }
 tok_iff                { Oper "<=>"}
 tok_impliedby          { Oper "<=" }
 tok_implies            { Oper "=>"}
 tok_nand               { Oper "~&"}
 tok_nequals            { Oper "!=" }
 tok_nor                { Oper "~|"}
 tok_xor                { Oper "<~>"}

 tok_ampersand          { Oper "&" }
 tok_exclam             { Oper "!" }
 tok_question           { Oper "?" }
 tok_tilde              { Oper "~" }
 tok_vline              { Oper "|" }

 tok_ac                 { LowerWord "ac" }
 tok_assumptions        { LowerWord "assumptions" }
 tok_cnf                { LowerWord "cnf" }
 tok_creator            { LowerWord "creator" }
 tok_description        { LowerWord "description" }
 tok_equality           { LowerWord "equality" }
 tok_file               { LowerWord "file" }
 tok_fof                { LowerWord "fof" }
 tok_include_           { LowerWord "include" }
 tok_inference          { LowerWord "inference" }
 tok_introduced         { LowerWord "introduced" }
 tok_iquote             { LowerWord "iquote" }
 tok_refutation         { LowerWord "refutation" }
 tok_status             { LowerWord "status" }
 tok_theory             { LowerWord "theory" }

 tok_fd_cnf             { DollarWord "$cnf" }
 tok_fd_fof             { DollarWord "$fof" }
 tok_fd_fot             { DollarWord "$fot" }

 tok_distinct_object    { DoubleQuoted $$ }
 tok_dollar_dollar_word { DollarDollarWord $$ }
 tok_dollar_word        { DollarWord $$ }
 tok_lower_word         { LowerWord $$ }
 tok_real               { Real $$ }
 tok_signed_integer     { SignedInt $$ }
 tok_single_quoted      { SingleQuoted $$ }
 tok_slash              { Slash }
 tok_unsigned_integer   { UnsignedInt $$ }
 tok_upper_word         { UpperWord $$ }

 comment                { CommentToken $$ }

%%

TSTP_file :: {[F]}
TSTP_file : {[]} | TSTP_input TSTP_file  {$1 : $2}

TSTP_input :: {F}
TSTP_input : annotated_formula  {$1}
--             | include  { $1 }
--             | comment { Comment $1 }

annotated_formula :: {F}
annotated_formula : cnf_annotated  {$1}
                  | fof_annotated  {$1}

fof_annotated :: {F}
fof_annotated : fof lp name  comma formula_role  comma fof_formula  annotations  rp dot
              { F { formula = $7
                  , name    = readWord $3
                  , role    = $5
                  , source  = $8
                  }
              }

cnf_annotated :: {F}
cnf_annotated : cnf lp name  comma formula_role  comma cnf_formula  annotations  rp dot
              { F { formula = univquantFreeVars $7
                  , name    = readWord $3
                  , role    = $5
                  , source  = $8
                  }
              }
--

annotations :: {Source}
annotations : comma source optional_info {$2}
            | { NoSource }

formula_role :: {Role}
formula_role : lower_word_ { readRole $1 }

-- fof_formula

fof_formula :: {Formula}
fof_formula : binary_formula  {$1}
            | unitary_formula {$1}


binary_formula :: {Formula}
binary_formula : nonassoc_binary {$1}
               | assoc_binary    {$1}

nonassoc_binary :: {Formula}
nonassoc_binary : unitary_formula  binary_connective  unitary_formula
                  { BinOp $1 $2 $3 }

assoc_binary :: {Formula}
assoc_binary : and_formula {$1}
             | or_formula  {$1}


or_formula :: {Formula}
or_formula : unitary_formula   vline  unitary_formula  more_or_formula
           { L.foldl (binOp (:|:)) (BinOp $1 (:|:) $3) $4 }

more_or_formula :: {[Formula]}
more_or_formula : {[]} | vline  unitary_formula more_or_formula
                 { $2 : $3 }

and_formula :: {Formula}
and_formula : unitary_formula  ampersand unitary_formula  more_and_formula
            { L.foldl (binOp (:&:)) (BinOp $1 (:&:) $3) $4 }


more_and_formula :: {[Formula]}
more_and_formula : {[]} | ampersand unitary_formula more_and_formula
                   { $2 : $3 }

unitary_formula :: {Formula}
unitary_formula : atomic_formula      {$1}
                | lp fof_formula  rp  {$2}
                | quantified_formula  {$1}
                | unary_formula       {$1}

quantified_formula :: {Formula}
quantified_formula : quantifier  lbra variable_list  rbra colon unitary_formula
                     { Quant $1 $3 $6 }

variable_list :: {[V]}
variable_list : variable {[$1]}
              | variable comma variable_list  { $1 : $3 }

--TODO
unary_formula :: {Formula}
unary_formula : fol_infix_unary  {$1}
              | tilde  unitary_formula  { (:~:) $2 }


-- cnf_formula :: {Formula}
-- cnf_formula : assoc_binary {$1}
--             | lp assoc_binary rp {$2}

cnf_formula :: {Formula}
cnf_formula : disjunction         {$1}
            | lp disjunction  rp  {$2}


disjunction :: {Formula}
disjunction : literal  more_disjunction
            { L.foldl (binOp (:|:)) $1 $2 }

more_disjunction :: {[Formula]}
more_disjunction : {[]}
                 | vline literal more_disjunction
                 { $2 : $3 }

literal :: {Formula}
literal : atomic_formula  {$1}
        | tilde atomic_formula  { (:~:) $2}
        | fol_infix_unary  {$1}

fol_infix_unary :: {Formula}
fol_infix_unary : term  infix_inequality  term  { InfixPred $1 $2 $3 }

quantifier :: {Quant}
quantifier : exclam { All } | question { Exists }

binary_connective :: {BinOp}
binary_connective : iff       {(:<=>:)}
                  | impliedby {(:<=:) }
                  | implies   {(:=>:) }
                  | nand      {(:~&:) }
                  | nor       {(:~|:) }
                  | xor       {(:<~>:)}

atomic_formula :: {Formula}
atomic_formula : defined_atomic_formula  {$1}
               | plain_atomic_formula    {$1}
               | system_atomic_formula   {$1}


plain_atomic_formula :: {Formula}
plain_atomic_formula : plain_term { fApp2pApp $1 }

-- plain_atomic_formula  :== proposition  | predicate  lp arguments  rp
-- proposition  :== predicate
-- predicate  :== atomic_word

defined_atomic_formula :: {Formula}
defined_atomic_formula : defined_infix_formula  {$1}
                       | defined_plain_formula  {$1}

defined_plain_formula :: {Formula}
defined_plain_formula : defined_plain_term  {fApp2pApp $1}

--defined_plain_formula  :== defined_prop  | defined_pred  lp arguments  rp
--defined_prop  :== atomic_defined_word
--defined_prop  :== $true | $false
--defined_pred  :== atomic_defined_word
--defined_pred  :== $equal


defined_infix_formula :: {Formula}
defined_infix_formula : term  defined_infix_pred  term  { InfixPred $1 $2 $3 }

defined_infix_pred ::  {InfixPred}
defined_infix_pred : infix_equality { $1 }

infix_equality :: {InfixPred}
infix_equality : equals { (:=:) }

infix_inequality :: {InfixPred}
infix_inequality : nequals {(:!=:)}

system_atomic_formula :: {Formula}
system_atomic_formula : system_term {fApp2pApp $1}

term :: {Term}
term : function_term  {$1}
     | variable       {Var $1}

function_term :: {Term}
function_term : defined_term  {$1}
              | plain_term    {$1}
              | system_term   {$1}

plain_term :: {Term}
plain_term : constant                  {FunApp $1 []}
           | functor  lp arguments  rp {FunApp $1 $3}

constant :: {AtomicWord}
constant : functor {$1}

functor :: {AtomicWord}
functor : atomic_word {$1}

defined_term :: {Term}
defined_term : defined_atom {$1}
             | defined_atomic_term {$1}

defined_atom :: {Term}
defined_atom : distinct_object {DistinctObjectTerm (stripQuotes '"' $1)}
             | number {NumberLitTerm $1}

defined_atomic_term :: {Term}
defined_atomic_term : defined_plain_term {$1}

defined_plain_term :: {Term}
defined_plain_term : defined_constant                 {FunApp (AtomicWord $1) []}
                   | defined_functor lp arguments  rp {FunApp (AtomicWord $1) $3}

defined_constant :: {String}
defined_constant : defined_functor {$1}

defined_functor :: {String}
defined_functor : atomic_defined_word {$1}

system_term :: {Term}
system_term : system_constant                  {FunApp (AtomicWord $1) []}
            | system_functor  lp arguments  rp {FunApp (AtomicWord $1) $3}

system_constant :: {String}
system_constant : system_functor  {$1}

system_functor :: {String}
system_functor : atomic_system_word {$1}

variable :: {V}
variable : upper_word {V $1}

arguments :: {[Term]}
arguments : term {[$1]}
          | term comma arguments { $1 : $3 }

source :: {Source}
source : dag_source       {$1}
       | internal_source  {$1}
       | external_source  {$1}

dag_source :: {Source}
dag_source : name  {Name (readWord $1)}
           | inference_record {$1}

-- Inference

inference_record :: {Source}
inference_record : inference lp inference_rule  comma useful_info  comma lbra parent_list rbra rp
                 { Inference $3 $5 $8 }

inference_rule :: {Rule}
inference_rule : atomic_word { readRule $ readWord $1 }

parent_list :: {[Parent]}
parent_list  : parent_info {[$1]}
             | parent_info comma parent_list { $1 : $3 }

parent_info :: {Parent}
parent_info : atomic_word parent_details { Parent (readWord $1) $2 }

parent_details :: { [GTerm] }
parent_details : colon general_list  {$2}
               | {[]}

internal_source :: {Source}
internal_source : introduced lp intro_type optional_info rp
                { Introduced $3 $4 }

intro_type :: {IntroType}
intro_type : lower_word_ {readType $1}

external_source :: {Source}
external_source : file_source    {$1}
                | theory_         {$1}
                | creator_source {$1}

file_source :: {Source}
file_source : file lp file_name file_info rp
            { File    $3        $4 }

file_info :: {Maybe String}
file_info : comma name {Just (readWord $2)}
          | {Nothing}

theory_ :: {Source}
theory_ : theory lp theory_name optional_info rp
        { Theory    $3          $4 }

theory_name :: {Theory}
theory_name : equality { Equality }
            | ac       { AC }

creator_source :: {Source}
creator_source : creator lp creator_name optional_info  rp
               { Creator $3 $4 }

creator_name :: {String}
creator_name : atomic_word {readWord $1}

optional_info :: {[Info]}
optional_info : comma useful_info  {$2} | {[]}

-- useful_info  :: { UsefulInfo }
-- useful_info  : general_list  {UsefulInfo $1}

useful_info :: {[Info]}
useful_info : lbra info_items rbra {$2}
            | lbra rbra {[]}

info_items :: {[Info]}
info_items : info_item                    {[$1]}
           | info_item  comma info_items  {$1 : $3}

info_item :: {Info}
info_item : formula_item     {$1}
          | inference_item   {$1}
          | general_function {$1}


general_function :: {Info}
general_function :  atomic_word lp general_terms rp { Function (readWord $1) $3}


formula_item :: {Info}
formula_item : description_item {$1}
             | iquote_item      {$1}

description_item :: {Info}
description_item : description lp atomic_word  rp { Description $ readWord $3 }

iquote_item :: {Info}
iquote_item : iquote lp atomic_word  rp { IQuote $ readWord $3 }

inference_item :: {Info}
inference_item : inference_status    {$1}
               | assumptions_record  {$1}
               | refutation_         {$1}


inference_status :: {Info}
inference_status : status lp status_value  rp { Status $3 }
                 | inference_info { $1 }


status_value :: {Status}
status_value : lower_word_ { readStatus $1 }


inference_info :: {Info}
inference_info : inference_rule  lp atomic_word  comma general_list  rp
                 { InferenceInfo $1 ( readWord $3) $5 }


assumptions_record :: {Info}
assumptions_record : assumptions lp lbra name_list rbra rp {AssumptionR (L.map readWord $4)}


refutation_ :: {Info}
refutation_ : refutation lp file_source  rp { Refutation $3 }

-- include :: {TPTP_Input}
-- include  : include_ lp file_name formula_selection  rp dot { Include $3 $4 }

-- formula_selection  :: {[AtomicWord]}
-- formula_selection  :  comma lbra name_list  rbra { $3 }
--                    |   { [] }

name_list :: {[AtomicWord]}
name_list : name  {[$1]}
          | name  comma name_list  { $1 : $3 }

general_term :: {GTerm}
general_term : general_data  {GTerm $1}
             | general_data colon general_term  {ColonSep $1 $3}
             | general_list {GList $1}

general_data :: {GData}
general_data : atomic_word  lp general_terms  rp { GApp $1 $3 }
             | atomic_word  { GWord $1 }
             | distinct_object { GDistinctObject (stripQuotes '"' $1) }
             | formula_data  { $1 }
             | number  { GNumber $1 }
             | variable  { GVar $1 }


formula_data :: {GData}
formula_data : tok_fd_cnf lp cnf_formula rp { GFormulaData "$cnf" $3 }
             | tok_fd_fof lp fof_formula rp { GFormulaData "$fof" $3 }
             | tok_fd_fot lp term rp        { GFormulaTerm "$fot" $3 }

general_list :: {[GTerm]}
general_list : lbra rbra {[]}
             | lbra general_terms  rbra {$2}

general_terms :: {[GTerm]}
general_terms : general_term  {[$1]}
              | general_term  comma general_terms  {$1 : $3}

name :: {AtomicWord}
name : atomic_word  {$1}
     | unsigned_integer {AtomicWord(show $1)}

atomic_word :: {AtomicWord}
atomic_word : lower_word_ {AtomicWord $1}
            | single_quoted{AtomicWord (stripQuotes '\'' $1)}

atomic_defined_word :: {String}
atomic_defined_word : dollar_word{$1}

atomic_system_word :: {String}
atomic_system_word : dollar_dollar_word{$1}

number :: {Rational} -- maybe keep track of the number type that was actually parsed
number : integer {fromIntegral $1} | rational {$1} | real {$1}

integer :: {Integer}
integer : signed_integer {$1} | unsigned_integer {$1}

rational :: {Rational}
rational : integer tok_slash unsigned_integer {$1 % $3}

file_name :: {String}
file_name : single_quoted {stripQuotes '\'' $1}

lower_word_ :: {String}
lower_word_ : cnf {"cnf"}
            | dollarCnf {"$cnf"}
            | fof {"fof"}
            | include_ {"include"} -- "fof" is a perfectly cromulent lower_word, but it is interpreted as a "fof" token
            | lower_word {$1}

--

lp                 :: {Token}
lp                 : tok_lp                  comment_list { $1 }

rp                 :: {Token}
rp                 : tok_rp                  comment_list { $1 }

lbra               :: {Token}
lbra               : tok_lbra                comment_list { $1 }

rbra               :: {Token}
rbra               : tok_rbra                comment_list { $1 }

comma              :: {Token}
comma              : tok_comma               comment_list { $1 }

dot                :: {Token}
dot                : tok_dot                 comment_list { $1 }

colon              :: {Token}
colon              : tok_colon               comment_list { $1 }

iff                :: {Token}
iff                : tok_iff                 comment_list { $1 }

implies            :: {Token}
implies            : tok_implies             comment_list { $1 }

xor                :: {Token}
xor                : tok_xor                 comment_list { $1 }

nor                :: {Token}
nor                : tok_nor                 comment_list { $1 }

nand               :: {Token}
nand               : tok_nand                comment_list { $1 }

impliedby          :: {Token}
impliedby          : tok_impliedby           comment_list { $1 }

equals             :: {Token}
equals             : tok_equals              comment_list { $1 }

nequals            :: {Token}
nequals            : tok_nequals             comment_list { $1 }

exclam             :: {Token}
exclam             : tok_exclam              comment_list { $1 }

question           :: {Token}
question           : tok_question            comment_list { $1 }

ampersand          :: {Token}
ampersand          : tok_ampersand           comment_list { $1 }

vline              :: {Token}
vline              : tok_vline               comment_list { $1 }

tilde              :: {Token}
tilde              : tok_tilde               comment_list { $1 }

fof                :: {Token}
fof                : tok_fof                 comment_list { $1 }

cnf                :: {Token}
cnf                : tok_cnf                 comment_list { $1 }


dollarCnf          :: {Token}
dollarCnf          : tok_fd_cnf              comment_list { $1 }

include_           :: {Token}
include_           : tok_include_            comment_list { $1 }

single_quoted      :: {String}
single_quoted      : tok_single_quoted       comment_list { $1 }

distinct_object    :: {String}
distinct_object    : tok_distinct_object     comment_list { $1 }

dollar_word        :: {String}
dollar_word        : tok_dollar_word         comment_list { $1 }

dollar_dollar_word :: {String}
dollar_dollar_word : tok_dollar_dollar_word  comment_list { $1 }

upper_word         :: {String}
upper_word         : tok_upper_word          comment_list { $1 }

lower_word         :: {String}
lower_word         : tok_lower_word          comment_list { $1 }

signed_integer     :: {Integer}
signed_integer     : tok_signed_integer      comment_list { $1 }

unsigned_integer   :: {Integer}
unsigned_integer   : tok_unsigned_integer    comment_list { $1 }

real               :: {Rational}
real               : tok_real                comment_list { $1 }

--

inference          :: {Token}
inference          : tok_inference           comment_list {$1}

file               :: {Token}
file               : tok_file                comment_list {$1}

theory             :: {Token}
theory             : tok_theory              comment_list {$1}

introduced         :: {Token}
introduced         : tok_introduced          comment_list {$1}

ac                 :: {Token}
ac                 : tok_ac                  comment_list {$1}

equality           :: {Token}
equality           : tok_equality           comment_list {$1}

creator            :: {Token}
creator            : tok_creator             comment_list {$1}

iquote             :: {Token}
iquote             : tok_iquote              comment_list {$1}

status             :: {Token}
status             : tok_status              comment_list {$1}

assumptions        :: {Token}
assumptions        : tok_assumptions         comment_list {$1}

refutation         :: {Token}
refutation         : tok_refutation          comment_list {$1}

description        :: {Token}
description        : tok_description         comment_list {$1}

comment_list :: {[String]}
comment_list : {[]} | comment comment_list { $1 : $2 }

{

stripQuotes which (x:xs) = go xs
  where
    go [x] = []
    go ('\\':'\\':xs) = '\\':go xs
    go ('\\':which:xs) = which:go xs
    go (x:xs) = x:go xs

fApp2pApp (FunApp x args) = PredApp x args

}
