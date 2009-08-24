module Main where

import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server.SimpleHTTP
import Control.Monad
import Control.Monad.Trans(liftIO)
import System.Exit(exitSuccess)
import Control.Concurrent(forkIO, killThread)
import Data.List

import App.Migrations(createTables)
import Framework.Database.Model
import qualified App.Models.TVShow as TVShow
import Framework.Database.Database(connectDatabase)
import Framework.AppState

main = do
    db <- connectDatabase
    createTables db

    let conf = nullConf { port = 8080 }
    -- httpTid <- forkIO $ simpleHTTP' (runApp) conf paths
    simpleHTTP' (runApp) conf paths
    -- killThread httpTid

paths = requestWithDatabase $ msum [
      dir "test"   $ doTest
    , dir "dbtest" $ doDbTest
    , dir "shows"  $ doShows
    , welcome
    ]

-- doTest ::  ServerPartT (StateT App IO) Response
doTest ::  AppServerPartT Response
doTest = do
    (return.toResponse) "Test"

doDbTest ::  AppServerPartT Response
doDbTest = do
    db <- getDatabase
    liftIO $ do stmnt <- prepare db "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY AUTOINCREMENT, name VARCHAR(255))"
                execute stmnt []
                stmnt2 <- prepare db "INSERT INTO test (name) VALUES ('Item 1')"
                execute stmnt2 []
                commit db
    (return.toResponse) "Database test"

doShows ::  AppServerPartT Response
doShows = do
    db <- getDatabase
    shows <- liftIO $ do 
        stmnt <- prepare db "SELECT * FROM tvshows ORDER BY name"
        execute stmnt []
        rows <- fetchAllRowsAL' stmnt
        return $  (fromRows rows :: [TVShow.TVShow])
    (return.toResponse) $ intercalate "<br />\n" (map (\s -> TVShow.name s ++ ", " ++ TVShow.description s ++ ", " ++ TVShow.url s) shows)

welcome ::  AppServerPartT Response
welcome = do
    name <- getAppName
    (return.toResponse) $ "Welcome to " ++ name ++ "!"









