{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Drifter.PostgreSQL
    ( PGMigration
    , Method(..)
    , DBConnection(..)
    , ChangeHistory(..)
    , runMigrations
    , getChangeHistory
    , getChangeNameHistory
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative                  as A
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Drifter
-------------------------------------------------------------------------------


data PGMigration


data instance Method PGMigration = MigrationQuery Query
                                 -- ^ Run a query against the database
                                 | MigrationCode (Connection -> IO (Either String ()))
                                 -- ^ Run any arbitrary IO code


data instance DBConnection PGMigration = DBConnection PGMigrationConnection


data PGMigrationConnection = PGMigrationConnection (Set ChangeName) Connection


instance Drifter PGMigration where
  migrateSingle (DBConnection migrationConn) change = do
    runExceptT $ migrateChange migrationConn change


-------------------------------------------------------------------------------
-- Change History Tracking
-------------------------------------------------------------------------------
newtype ChangeId = ChangeId Int deriving (Eq, Ord, Show, FromField)


data ChangeHistory = ChangeHistory {
      histId          :: ChangeId
    , histName        :: ChangeName
    , histDescription :: Maybe Description
    , histTime        :: UTCTime
    } deriving (Show)


instance Eq ChangeHistory where
    a == b = (histName a) == (histName b)


instance Ord ChangeHistory where
    compare a b = compare (histId a) (histId b)


instance FromRow ChangeHistory where
    fromRow = ChangeHistory <$> field
                            <*> (ChangeName <$> field)
                            <*> field
                            <*> field


-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------
bootstrapQ :: Query
bootstrapQ = [sql|
CREATE TABLE IF NOT EXISTS schema_migrations (
    id              serial      NOT NULL,
    name            text        NOT NULL,
    description     text,
    time            timestamptz NOT NULL DEFAULT now(),

    PRIMARY KEY (id),
    UNIQUE (name)
);
|]


-------------------------------------------------------------------------------
changeHistoryQ :: Query
changeHistoryQ =
  "SELECT id, name, description, time FROM schema_migrations ORDER BY id;"


-------------------------------------------------------------------------------
changeNameHistoryQ :: Query
changeNameHistoryQ =
  "SELECT name FROM schema_migrations ORDER BY id;"


-------------------------------------------------------------------------------
insertLogQ :: Query
insertLogQ =
  "INSERT INTO schema_migrations (name, description, time) VALUES (?, ?, ?);"


-------------------------------------------------------------------------------
migrateChange :: PGMigrationConnection -> Change PGMigration -> ExceptT String IO ()
migrateChange (PGMigrationConnection hist c) ch@Change{..} = do
  if Set.member changeName hist
    then lift $ putStrLn $ "Skipping: " ++ show (changeNameText changeName)
    else do
      runMethod c changeMethod
      logChange c ch
      lift $ putStrLn $ "Committed: " ++ show changeName


-------------------------------------------------------------------------------
runMethod :: Connection -> Method PGMigration -> ExceptT String IO ()
runMethod c (MigrationQuery q) =
  void $ ExceptT $ (Right <$> execute_ c q) `catches` errorHandlers
runMethod c (MigrationCode f) =
  ExceptT $ f c `catches` errorHandlers


  -------------------------------------------------------------------------------
logChange :: Connection -> Change PGMigration -> ExceptT String IO ()
logChange c Change{..} = do
    now <- lift getCurrentTime
    void $ ExceptT $ (Right <$> go now) `catches` errorHandlers
  where
    go now = execute c insertLogQ (changeNameText changeName, changeDescription, now)


-------------------------------------------------------------------------------
errorHandlers :: [Handler (Either String b)]
errorHandlers = [ Handler (\(ex::SqlError) -> return $ Left $ show ex)
                , Handler (\(ex::FormatError) -> return $ Left $ show ex)
                , Handler (\(ex::ResultError) -> return $ Left $ show ex)
                , Handler (\(ex::QueryError) -> return $ Left $ show ex)
                ]


-------------------------------------------------------------------------------
-- | Takes a connection and builds the state to thread throughout the migration.
-- This includes bootstrapping the migration tables and collecting all the
-- migrations that have already been committed.
makePGMigrationConnection :: Connection -> IO PGMigrationConnection
makePGMigrationConnection conn = do
  void $ execute_ conn bootstrapQ
  hist <- getChangeNameHistory conn
  return $ PGMigrationConnection (Set.fromList hist) conn


-------------------------------------------------------------------------------
-- | Takes the list of all migrations, removes the ones that have
-- already run and runs them. Use this instead of 'migrate'.
runMigrations :: Connection -> [Change PGMigration] -> IO (Either String ())
runMigrations conn changes = do
  begin conn
  migrationConn <- makePGMigrationConnection conn
  res <- migrate (DBConnection migrationConn) changes `onException` rollback conn
  case res of
    Right _ -> commit conn
    Left  _ -> rollback conn
  return res


-------------------------------------------------------------------------------
-- | Get all changes from schema_migrations table for all the migrations that
-- have previously run.
getChangeHistory :: Connection -> IO [ChangeHistory]
getChangeHistory conn = query_ conn changeHistoryQ


-------------------------------------------------------------------------------
-- | Get just the names of all changes from schema_migrations for migrations
-- that have previously run.
getChangeNameHistory :: Connection -> IO [ChangeName]
getChangeNameHistory conn = fmap (\(Only nm) -> ChangeName nm)
                        A.<$> query_ conn changeNameHistoryQ

