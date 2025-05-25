-- From ./hackage-roundtrip-work/postgresql-query-3.10.0/src/Database/PostgreSQL/Query/TH/Row.hs
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module Database.PostgreSQL.Query.TH.Row
  ( deriveFromRow
  , deriveToRow
  ) where

import Database.PostgreSQL.Query.TH.Common
import Database.PostgreSQL.Simple.FromRow ( FromRow(..), field )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Language.Haskell.TH

{-| Derive 'FromRow' instance. i.e. you have type like that

@
data Entity = Entity
              { eField :: Text
              , eField2 :: Int
              , efield3 :: Bool }
@

then 'deriveFromRow' will generate this instance:
instance FromRow Entity where

@
instance FromRow Entity where
    fromRow = Entity
              \<$> field
              \<*> field
              \<*> field
@

Datatype must have just one constructor with arbitrary count of fields
-}

deriveFromRow :: Name -> Q [Dec]
deriveFromRow t = do
    con <- dataConstructors <$> reify t >>= \case
      [a] -> return a
      x -> fail $ "expected exactly 1 data constructor, but " ++ show (length x) ++ " got"
    cname <- cName con
    cargs <- cArgs con
    [d|instance FromRow $(return $ ConT t) where
           fromRow = $(fieldsQ cname cargs)|]
  where
    fieldsQ cname cargs = do
        fld <- [| field |]
        fmp <- [| (<$>) |]
        fap <- [| (<*>) |]
        return $ UInfixE (ConE cname) fmp (fapChain cargs fld fap)

    fapChain 0 _ _ = error "there must be at least 1 field in constructor"
    fapChain 1 fld _ = fld
    fapChain n fld fap = UInfixE fld fap (fapChain (n-1) fld fap)

{-| derives 'ToRow' instance for datatype like

@
data Entity = Entity
              { eField :: Text
              , eField2 :: Int
              , efield3 :: Bool }
@

it will derive instance like that:

@
instance ToRow Entity where
     toRow (Entity e1 e2 e3) =
         [ toField e1
         , toField e2
         , toField e3 ]
@
-}

deriveToRow :: Name -> Q [Dec]
deriveToRow t = do
    con <- dataConstructors <$> reify t >>= \case
      [a] -> return a
      x -> fail $ "expected exactly 1 data constructor, but " ++ show (length x) ++ " got"
    cname <- cName con
    cargs <- cArgs con
    cvars <- sequence
             $ replicate cargs
             $ newName "a"
    [d|instance ToRow $(return $ ConT t) where
#if MIN_VERSION_template_haskell(2,18,0)
           toRow $(return $ ConP cname [] $ map VarP cvars) = $(toFields cvars)|]
#else
           toRow $(return $ ConP cname $ map VarP cvars) = $(toFields cvars)|]
#endif
  where
    toFields v = do
        tof <- lookupVNameErr "toField"
        return $ ListE $ map (\e -> AppE (VarE tof) (VarE e)) v

