module Framework.AppState.Database (
  -- * Highlevel database access.
  getDatabase
, requestWithDatabase
  -- * Query functions.
, fetchAll
, fetchAllModels
) where

import Database.HDBC
import Framework.Database.Model (DBModel, RowAssocList, fromRows)
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
                           getDatabase >>= liftIO.disconnect
                           return res



-- | Wrapper to execute a prepared statement and return results as 'RowAssocList'.
fetchAll :: String -> [SqlValue] -> AppServerPartT [RowAssocList]
fetchAll query params = do
    db <- getDatabase
    liftIO $ do
        stmnt <- prepare db query
        execute stmnt params
        rows <- fetchAllRowsAL stmnt
        return rows

-- | Like 'fetchAll', but return results as a list of DBModel instances.
-- Might have to use extra typehints to tell the compiler which types are expected.
fetchAllModels :: (DBModel a) => String-> [SqlValue]-> AppServerPartT [a]
fetchAllModels query params = return . fromRows =<< fetchAll query params



