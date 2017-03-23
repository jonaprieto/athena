
-- | Utilities for pretty printing.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Athena.Utils.PrettyPrint
  ( module Text.PrettyPrint
  , bquotes
  , Pretty(pretty)
  , prettyShow
  , spaces
  , sspaces
  , squotes
  ) where

------------------------------------------------------------------------------

import Text.PrettyPrint

------------------------------------------------------------------------------

-- | Wrap a document in ‘...’.
bquotes ∷ Doc → Doc
bquotes d = char '‘' <> d <> char '’'

-- | Wrap a document in spaces.
spaces ∷ Doc → Doc
spaces d = space <> d <> space

-- | Wrap a string in ‘...’.
squotes ∷ String → Doc
squotes = bquotes . text

-- | Wrap a string in spaces.
sspaces ∷ String → Doc
sspaces = spaces . text


-- | Use instead of 'show' when printing to world.
prettyShow ∷ Pretty a ⇒ a → String
prettyShow = render . pretty

------------------------------------------------------------------------------

-- | Pretty print type class.
class Pretty a where
  pretty ∷ a → Doc

instance Pretty Doc where
  pretty = id

instance Pretty String where
  pretty = text
