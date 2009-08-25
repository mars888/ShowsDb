module Main where

import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server.SimpleHTTP
import Happstack.Server.HTTP.FileServe(fileServe)
import Control.Monad
import Control.Monad.Trans(liftIO)
import System.Exit(exitSuccess)
import Control.Concurrent(forkIO, killThread)
import Data.List
import Text.StringTemplate
import Text.StringTemplate.Classes

import App.Migrations(createTables)
import Framework.Database.Model
import qualified App.Models.TVShow as TVShow
import Framework.Database.Database(connectDatabase)
import Framework.AppState

main = do
    db <- connectDatabase
    createTables db

    let conf = nullConf { port = 8080 }
    httpTid <- forkIO $ simpleHTTP' (runApp) conf paths
    -- simpleHTTP' (runApp) conf paths
    -- killThread httpTid
    return httpTid

paths = requestWithDatabase $ msum [
      dir "test"  $ doTest
    , dir "shows" $ showsIndex
    , staticServe "public"
    , welcome
    ]
    where staticServe d = dir d (fileServe [] d)

-- doTest ::  ServerPartT (StateT App IO) Response
doTest ::  AppServerPartT Response
doTest = do
    (return.toResponse) "Test"

showsIndex ::  AppServerPartT Response
showsIndex = do
    asHtml
    db <- getDatabase
    shows <- liftIO $ do
        stmnt <- prepare db "SELECT * FROM tvshows ORDER BY name"
        execute stmnt []
        rows <- fetchAllRowsAL' stmnt
        return $ (fromRows rows :: [TVShow.TVShow])
    templateWith "shows_index" (assign "shows" shows)

welcome ::  AppServerPartT Response
welcome = do
    asHtml
    template "welcome_index"









