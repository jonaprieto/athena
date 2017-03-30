
-- | Data.TSTP.Parent module.
-- Adapted from https://github.com/agomezl/tstp2agda.

module Data.TSTP.Parent where

------------------------------------------------------------------------------

import Data.TSTP.GData ( GTerm(..) )

------------------------------------------------------------------------------

-- | Parent formula information.
data Parent = Parent String [GTerm]
              deriving (Eq, Ord, Show, Read)
