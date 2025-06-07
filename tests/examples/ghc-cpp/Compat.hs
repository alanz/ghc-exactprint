-- Originally ./hackage-roundtrip-work/base-compat-0.14.1/src/Type/Reflection/Compat.hs

{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
# if !(MIN_VERSION_base(4,11,0))
{-# LANGUAGE TypeInType #-}
# endif
#endif
module Type.Reflection.Compat (
#if MIN_VERSION_base(4,10,0)
  module Base
, withTypeable
, pattern TypeRep
, decTypeRep
#endif
) where

#if MIN_VERSION_base(4,11,0)
import Type.Reflection as Base
#elif MIN_VERSION_base(4,10,0)
import Type.Reflection as Base hiding (withTypeable)
#endif

#if MIN_VERSION_base(4,10,0)
# if !(MIN_VERSION_base(4,11,0))
import GHC.Exts (TYPE)
import Type.Reflection (Typeable, TypeRep)
# endif

# if !(MIN_VERSION_base(4,19,0))
import Data.Void (Void)
import Prelude.Compat
import Type.Reflection.Unsafe (typeRepFingerprint)
import Unsafe.Coerce (unsafeCoerce)
# endif

# if !(MIN_VERSION_base(4,11,0))
-- | Use a 'TypeRep' as 'Typeable' evidence.
withTypeable :: forall (a :: k) (r :: TYPE rep). ()
             => TypeRep a -> (Typeable a => r) -> r
withTypeable rep k = unsafeCoerce k' rep
  where k' :: Gift a r
        k' = Gift k

-- | A helper to satisfy the type checker in 'withTypeable'.
newtype Gift a (r :: TYPE rep) = Gift (Typeable a => r)
# endif

# if !(MIN_VERSION_base(4,17,0))
-- | A 'TypeableInstance' wraps up a 'Typeable' instance for explicit
-- handling. For internal use: for defining 'TypeRep' pattern.
data TypeableInstance (a :: k) where
 TypeableInstance :: Typeable a => TypeableInstance a

-- | Get a reified 'Typeable' instance from an explicit 'TypeRep'.
--
-- For internal use: for defining 'TypeRep' pattern.
typeableInstance :: forall a. TypeRep a -> TypeableInstance a
typeableInstance rep = withTypeable rep TypeableInstance

-- | A explicitly bidirectional pattern synonym to construct a
-- concrete representation of a type.
--
-- As an __expression__: Constructs a singleton @TypeRep a@ given a
-- implicit 'Typeable a' constraint:
--
-- @
-- TypeRep @a :: Typeable a => TypeRep a
-- @
--
-- As a __pattern__: Matches on an explicit @TypeRep a@ witness bringing
-- an implicit @Typeable a@ constraint into scope.
--
-- @
-- f :: TypeRep a -> ..
-- f TypeRep = {- Typeable a in scope -}
-- @
--
-- /Since: 4.17.0.0/
pattern TypeRep :: forall a. () => Typeable a => TypeRep a
pattern TypeRep <- (typeableInstance -> TypeableInstance)
  where TypeRep = typeRep
# endif

# if !(MIN_VERSION_base(4,19,0))
-- | Type equality decision
--
-- /Since: 4.19.0.0/
decTypeRep :: forall k1 k2 (a :: k1) (b :: k2).
             TypeRep a -> TypeRep b -> Either (a :~~: b -> Void) (a :~~: b)
decTypeRep a b
  | sameTypeRep a b = Right (unsafeCoerce HRefl)
  | otherwise       = Left (\HRefl -> errorWithoutStackTrace ("decTypeRep: Impossible equality proof " ++ show a ++ " :~: " ++ show b))
{-# INLINEABLE decTypeRep #-}

sameTypeRep :: forall k1 k2 (a :: k1) (b :: k2).
               TypeRep a -> TypeRep b -> Bool
sameTypeRep a b = typeRepFingerprint a == typeRepFingerprint b
# endif
#endif

