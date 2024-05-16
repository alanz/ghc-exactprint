{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
module Env.Generic
  ( Record(..)
  , type (?)(..)
  ) where

import Data.Promotion.Prelude (type (:+$), type (:*$), type (:^$), type (:-$))
import Options.Generic (Generic, ParseRecord, type (<?>)(..))
