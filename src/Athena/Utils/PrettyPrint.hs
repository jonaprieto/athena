
-- | Athena.Utils.PrettyPrint module.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Athena.Utils.PrettyPrint
  ( module Text.PrettyPrint.HughesPJ
  , cquotes
  , Pretty(pretty)
  , prettyShow
  , scquotes
  , spaces
  , sspaces
  ) where

------------------------------------------------------------------------------

import Text.PrettyPrint.HughesPJ

------------------------------------------------------------------------------
-- Auxiliary functions

-- | Wrap a document in curly quotes (‘...’).
cquotes ∷ Doc → Doc
cquotes d = char '‘' <> d <> char '’'

-- | Wrap a document in spaces.
spaces ∷ Doc → Doc
spaces d = space <> d <> space

-- | Wrap a string in ‘...’.
scquotes ∷ String → Doc
scquotes = cquotes . text

-- | Wrap a string in spaces.
sspaces ∷ String → Doc
sspaces = spaces . text

-- | Use instead of 'show' when printing to world.
prettyShow :: Pretty a ⇒ a → String
prettyShow = render . pretty

------------------------------------------------------------------------------

-- | Pretty print type class.
class Pretty a where
  pretty ∷ a → Doc

instance Pretty Doc where
  pretty = id

instance Pretty String where
  pretty = text
