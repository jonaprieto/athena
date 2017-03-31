
-- | Athena.Utils.PrettyPrint module.
-- Adapted from https://github.com/asr/apia.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Athena.Utils.PrettyPrint
  ( module Text.PrettyPrint.HughesPJ
  , enclose
  , Pretty ( pretty )
  , prettyShow
  , squote
  , squotes
  ) where

------------------------------------------------------------------------------

import Text.PrettyPrint.HughesPJ

------------------------------------------------------------------------------

-- | The document @(enclose l r x)@ encloses document @x@ between
--   documents @l@ and @r@ using @(\<\>)@.
--
--   > enclose l r x = l <> x <> r
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r x = l <> x <> r

-- | The document @squote@ contains a single quote, \"'\".
squote :: Doc
squote = char '\''

-- | Document @(squotes x)@ encloses document @x@ with single quotes
--   \"'\".
squotes :: Doc -> Doc
squotes = enclose squote squote

-- | Use instead of 'show' when printing to world.
prettyShow :: Pretty a ⇒ a → String
prettyShow = render . pretty

-- | Pretty print type class.
class Pretty a where
  pretty ∷ a → Doc

instance Pretty Doc where
  pretty = id

instance Pretty String where
  pretty = text
