{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Based on https://www.haskell.org/haskellwiki/GHC/Type_families#Detailed_definition_of_data_families

module DataFamilies
  (
    GMap
  , GMapKey(type GMapK)
  ) where

-- Type 1, Top level

data family GMap k :: * -> *

data family Array e

data family ArrayK :: * -> *


-- Type 2, associated types

class GMapKey k where
  data GMapK k :: * -> *

class C a b c where { data T1 c a :: * }  -- OK
-- class C a b c where { data T a a :: * }  -- Bad: repeated variable
-- class D a where { data T a x :: * }      -- Bad: x is not a class variable
class D a where { data T2 a :: * -> * }   -- OK

-- Instances

data instance GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)

data family T3 a
data instance T3 Int  = A
data instance T3 Char = B
nonsense :: T3 a -> Int
-- nonsense A = 1             -- WRONG: These two equations together...
-- nonsense B = 2             -- ...will produce a type error.
nonsense = undefined


instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data GMapK (Either a b) v = GMapEitherK (GMap a v) (GMap b v)

-- data GMap () v = GMapUnit (Maybe v)
--               deriving Show


