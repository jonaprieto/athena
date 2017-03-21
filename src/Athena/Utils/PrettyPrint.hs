
-- | Utilities for pretty printing.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Utils.PrettyPrint
  ( module Text.PrettyPrint
  , bquotes
  , Pretty(pretty)
  , prettyShow
  -- , sspaces
  , squotes
  ) where

------------------------------------------------------------------------------

import Text.PrettyPrint

------------------------------------------------------------------------------

-- | Wrap a document in ‘...’.
bquotes ∷ Doc → Doc
bquotes d = char '‘' <> d <> char '’'

-- | Wrap a string in ‘...’.
squotes ∷ String → Doc
squotes = bquotes . text

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
