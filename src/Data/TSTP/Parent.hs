
-- | Data.TSTP.Parent module

module Data.TSTP.Parent where

------------------------------------------------------------------------------

import Data.TSTP.GData ( GTerm(..) )

------------------------------------------------------------------------------

-- | Parent formula information.
data Parent = Parent String [GTerm]
              deriving (Eq, Ord, Show, Read)
