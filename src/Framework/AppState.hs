module Framework.AppState where

import Happstack.Server.SimpleHTTP
import Control.Monad.Reader
import Database.HDBC(disconnect)
import Database.HDBC.Sqlite3(Connection)

import Framework.Database.Database(connectDatabase)
import Data.Maybe(fromJust)

data App = App {
      appName :: String
    , appDatabase :: Maybe Connection
    }

type AppState = ReaderT App IO
type AppServerPartT = ServerPartT AppState

initApp ::  App
initApp =
    App {
      appName = "Series Db"
    , appDatabase = Nothing
    }

runApp ::  AppState a -> IO a
runApp = flip runReaderT initApp

getApp ::  AppServerPartT App
getApp = lift ask

getAppName ::  AppServerPartT String
getAppName = asks appName

getDatabase ::  AppServerPartT Connection
getDatabase = lift (asks appDatabase) >>= return.fromJust

requestWithDatabase :: AppServerPartT b -> AppServerPartT b
requestWithDatabase f = do
    db  <- liftIO connectDatabase
    app <- getApp
    local (\app -> app { appDatabase = Just db }) runFunction
    where runFunction = do res <- f
                           getDatabase >>= liftIO.disconnect
                           return res

