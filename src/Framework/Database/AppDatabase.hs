module Framework.Database.AppDatabase where

import Control.Monad.Trans
import Control.Monad.Reader
import Database.HDBC.Sqlite3(Connection)
import Data.Maybe

import Framework.AppState
import Framework.Database.Database


-- getDb ::  AppServerPartT Connection
getAppName ::  AppServerPartT Connection
getAppName = asks appDatabase >>= return.fromJust

