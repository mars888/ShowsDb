module Framework.AppState.Database (
  -- * Highlevel database access.
  getDatabase
, requestWithDatabase
  -- * Query functions.
, commit
, execute
, execute'
, query
, fetchAll
, fetchAllModels
, fetchModel
  -- * Exports
, module Database.HDBC.SqlValue
, module Framework.Database.Sql
) where

import qualified Database.HDBC as HDBC
import Database.HDBC.SqlValue
import Framework.Database.Model (DBModel, RowAssocList, fromRow, fromRows)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (asks, local)
import Data.Maybe (fromJust)

import Framework.Database(CurrentConnection, connectDatabase)
import Framework.Database.Sql
import Framework.AppState.Types

getDatabase ::  AppServerPartT CurrentConnection
getDatabase = lift (asks appDatabase) >>= return.fromJust

requestWithDatabase :: AppServerPartT b -> AppServerPartT b
requestWithDatabase f = do
    db  <- liftIO connectDatabase
    app <- getApp
    local (\app -> app { appDatabase = Just db }) runFunction
    where runFunction = do res <- f
                           getDatabase >>= liftIO . HDBC.disconnect
                           return res

commit :: AppServerPartT ()
commit = do
    db <- getDatabase
    liftIO (HDBC.commit db)

execute' :: String -> [HDBC.SqlValue] -> AppServerPartT Integer
execute' query params = do
    db <- getDatabase
    liftIO $ do
        stmnt <- HDBC.prepare db query
        HDBC.execute stmnt params

execute :: (GenerateQuery a, GenerateParams a) => a -> AppServerPartT Integer
execute query = do
    db <- getDatabase
    liftIO $ do
        stmnt <- HDBC.prepare db (toQuery query)
        HDBC.execute stmnt (toParams query)

query :: (GenerateQuery a, GenerateParams a) => a -> AppServerPartT [RowAssocList]
query query = do
    db <- getDatabase
    liftIO $ do
        stmnt <- HDBC.prepare db (toQuery query)
        HDBC.execute stmnt (toParams query)
        rows <- HDBC.fetchAllRowsAL' stmnt
        return rows

-- | Wrapper to execute a prepared statement and return results as 'RowAssocList'.
fetchAll :: (GenerateQuery a, GenerateParams a) => a -> AppServerPartT [RowAssocList]
fetchAll query = do
    db <- getDatabase
    liftIO $ do
        stmnt <- HDBC.prepare db (toQuery query)
        HDBC.execute stmnt (toParams query)
        rows <- HDBC.fetchAllRowsAL' stmnt
        return rows

-- | Like 'fetchAll', but return results as a list of DBModel instances.
-- Might have to use extra typehints to tell the compiler which types are expected.
fetchAllModels :: (GenerateQuery a, GenerateParams a, DBModel b) => a -> AppServerPartT [b]
fetchAllModels query = return . fromRows =<< fetchAll query

fetchModel :: (GenerateQuery a, GenerateParams a, DBModel b) => a -> AppServerPartT (Maybe b)
fetchModel query = do
    items <- fetchAll query
    case items of
         x:_ -> return.Just $ (fromRow x)
         _   -> return Nothing


{-
-- | Wrapper to execute a prepared statement and return results as 'RowAssocList'.
fetchAll :: String -> [HDBC.SqlValue] -> AppServerPartT [RowAssocList]
fetchAll query params = do
    db <- getDatabase
    liftIO $ do
        stmnt <- HDBC.prepare db query
        HDBC.execute stmnt params
        rows <- HDBC.fetchAllRowsAL' stmnt
        return rows

-- | Like 'fetchAll', but return results as a list of DBModel instances.
-- Might have to use extra typehints to tell the compiler which types are expected.
fetchAllModels :: (DBModel a) => String-> [HDBC.SqlValue]-> AppServerPartT [a]
fetchAllModels query params = return . fromRows =<< fetchAll query params

fetchModel :: (DBModel a) => String-> [HDBC.SqlValue]-> AppServerPartT (Maybe a)
fetchModel query params = do
    items <- fetchAll query params
    case items of
         x:_ -> return.Just $ (fromRow x)
         _   -> return Nothing
-}
