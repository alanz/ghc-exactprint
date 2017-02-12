{-# LANGUAGE UndecidableInstances #-}
module Servant.Auth.Server.Internal.Class where

import Servant.Auth
import Data.Monoid
import Servant hiding (BasicAuth)

import Servant.Auth.Server.Internal.Types
import Servant.Auth.Server.Internal.ConfigTypes
import Servant.Auth.Server.Internal.BasicAuth
import Servant.Auth.Server.Internal.Cookie
import Servant.Auth.Server.Internal.JWT

-- | @IsAuth a ctx v@ indicates that @a@ is an auth type that expects all
-- elements of @ctx@ to be the in the Context and whose authentication check
-- returns an @AuthCheck v@.
class IsAuth a v  where
  type family AuthArgs a :: [*]
  runAuth :: proxy a -> proxy v -> Unapp (AuthArgs a) (AuthCheck v)

