module Framework.AppState.Database (
  -- * Highlevel database access.
  getDatabase
, requestWithDatabase
  -- * Query functions.
, commit
, query
, fetchAll
, fetchAllModels
, fetchModel
  -- * Exports
, module Database.HDBC.SqlValue
) where

import qualified Database.HDBC as HDBC
import Database.HDBC.SqlValue
import Framework.Database.Model (DBModel, RowAssocList, fromRow, fromRows)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (asks, local)
import Data.Maybe (fromJust)

import Framework.Database(CurrentConnection, connectDatabase)
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

query :: String -> [HDBC.SqlValue] -> AppServerPartT Integer
query query params = do
    db <- getDatabase
    liftIO $ do
        stmnt <- HDBC.prepare db query
        HDBC.execute stmnt params

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

