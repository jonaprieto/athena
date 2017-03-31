
-- | Athena.Utils.PrettyPrint module.
-- Adapted from https://github.com/asr/apia.
-- and http://hackage.haskell.org/package/wl-pprint-text-1.1.1.0/

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Athena.Utils.PrettyPrint
  ( module Text.PrettyPrint.HughesPJ
  , enclose
  , Pretty ( pretty )
  , linesep
  , prettyShow
  , putDoc
  , squote
  , squotes
  , (<$>)
  ) where

------------------------------------------------------------------------------

import Text.PrettyPrint.HughesPJ

------------------------------------------------------------------------------

infixr 5 <$>

-- | The document @(x \<$\> y)@ concatenates document @x@ and @y@ with
--   a 'line' in between. (infixr 5)
(<$>) :: Doc -> Doc -> Doc
(<$>) = splitWithLine False

-- | The document @(x \<$$\> y)@ concatenates document @x@ and @y@
--   with a 'linebreak' in between. (infixr 5)
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = splitWithLine True

splitWithLine :: Bool -> Doc -> Doc -> Doc
splitWithLine _ empty b     = b
splitWithLine _ a     empty = a
splitWithLine f a     b     = a <> Line f <> b

-- | The document @(enclose l r x)@ encloses document @x@ between
--   documents @l@ and @r@ using @(\<\>)@.
--
--   > enclose l r x = l <> x <> r
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r x = l <> x <> r

linesep :: Doc
linesep = hcat $ replicate 78 (char '-')

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


-- | The action @(putDoc doc)@ pretty prints document @doc@ to the
-- standard output, with a page width of 100 characters and a ribbon
-- width of 40 characters.
--
-- > main :: IO ()
-- > main = do{ putDoc (text "hello" <+> text "world") }
--
-- Which would output
--
-- @
-- hello world
-- @
putDoc :: Doc -> IO ()
putDoc doc = putStr $
  renderStyle
    Style { lineLength = 78
          , ribbonsPerLine = 1.5
          , mode = PageMode }
  doc
-- | Pretty print type class.
class Pretty a where
  pretty ∷ a → Doc

instance Pretty Doc where
  pretty = id

instance Pretty String where
  pretty = text
