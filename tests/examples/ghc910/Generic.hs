{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.Internal.Generic where


------------------------------------------------------------------------------------------
-- A Single type for all Options fields
------------------------------------------------------------------------------------------

-- | Type-level representation of the Aeson Generic deriving 'Options'.
--   This representation is useful for explicitly setting all options.
data GenericOptions
  :: fieldLabelModifier
  -> tagSingleConstructors
  -> Type

instance
  ( All StringFunction [fieldLabelModifier, constructorTagModifier]
  , ToSumEncoding sumEncoding
  , All KnownBool
     [ allNullaryToStringTag
     , tagSingleConstructors
     ]
  ) => ToAesonOptions
    (GenericOptions
      (FieldLabelModifier := fieldLabelModifier)
      (ConstructorTagModifier := constructorTagModifier)
      (TagSingleConstructors := tagSingleConstructors)) where
  toAesonOptions _           = defaultOptions
    { fieldLabelModifier     = stringFunction $ Proxy @fieldLabelModifier
    , tagSingleConstructors  = boolVal $ Proxy @tagSingleConstructors
    }



