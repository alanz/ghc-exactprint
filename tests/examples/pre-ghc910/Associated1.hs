{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

import qualified Data.IntMap
import Prelude hiding (lookup)
import Data.Char (ord)

class GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v
