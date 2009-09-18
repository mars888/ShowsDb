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
import Framework.Database(connectDatabase)
import Framework.AppState
import qualified App.Controllers.Shows as ShowsController

main = do
    db <- connectDatabase
    createTables db

    let conf = nullConf { port = 8080 }
    -- httpTid <- forkIO $ simpleHTTP' (runApp) conf paths
    simpleHTTP' (runApp) conf paths
    -- return httpTid

paths ::  AppServerPartT Response
paths = requestWithDatabase $ msum [
      dir "test"  $ doTest
    , dir "shows" $ ShowsController.paths
    , staticServe "public"
    , welcome
    ]
    where staticServe d = dir d (fileServe [] d)

-- doTest ::  ServerPartT (StateT App IO) Response
doTest ::  AppServerPartT Response
doTest = do
    (return.toResponse) "Test"

welcome ::  AppServerPartT Response
welcome = asHtml >> template "welcome_index"









