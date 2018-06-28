{-# LANGUAGE TypeInType, ExplicitForAll #-}

module BadTelescope3 where

import Data.Kind

data SameKind :: k -> k -> Type

type S a k (b :: k) = SameKind a b
