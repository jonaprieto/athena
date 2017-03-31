
-- | Athena.Utils.PrettyPrint module.
-- Adapted from https://github.com/asr/apia.

{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module Athena.Utils.PrettyPrint
  ( module Text.PrettyPrint.Leijen.Text
  , Pretty ( pretty )
  , squotes
  , spaces
  , sspaces
  ) where

------------------------------------------------------------------------------

import Data.Text.Lazy
import Text.PrettyPrint.Leijen.Text

------------------------------------------------------------------------------

-- | Wrap a document in spaces.
spaces ∷ Doc → Doc
spaces d = space <> d <> space

-- | Wrap a string in spaces.
sspaces ∷ String → Doc
sspaces = spaces . text . pack

instance Pretty String where
  pretty = text . pack
