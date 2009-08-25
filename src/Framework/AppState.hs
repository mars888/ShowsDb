module Framework.AppState where

import Happstack.Server.SimpleHTTP
import Control.Monad.State
import Control.Monad.Reader
import Database.HDBC(disconnect)
import Database.HDBC.Sqlite3(Connection)
import Text.StringTemplate
import Text.StringTemplate.Classes
import System.FilePath

import Framework.Database.Database(connectDatabase)
import Data.Maybe(fromJust)

data App = App {
      appName :: String
    , appDatabase :: Maybe Connection
    , templateGroup :: STGroup String
    }

type AppState = ReaderT App IO
type AppServerPartT = ServerPartT AppState

initApp ::  STGroup String -> App
initApp templateDirGroup = do
    App {
      appName = "Series Db"
    , appDatabase = Nothing
    , templateGroup = templateDirGroup
    }

--
-- App support
--
runApp ::  AppState a -> IO a
runApp appState = do
    dirGroup <- getDirectoryGroup
    flip runReaderT (initApp dirGroup) $ appState

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

--
-- Helpers
--
asHtml :: AppServerPartT ()
asHtml = setHeaderM "Content-type" "text/html"

--
-- Templates
--

-- Template state monad type.
type TemplateMonad = State  (StringTemplate String)

templateDir :: FilePath
templateDir = "templates"

getDirectoryGroup :: IO (STGroup String)
getDirectoryGroup = directoryGroup "templates"

getTemplate ::  String -> AppServerPartT (StringTemplate String)
getTemplate name = do
    dirGroup <- asks templateGroup
    return $ (fromJust.stGetFirst.dirGroup) name

template :: FilePath -> AppServerPartT Response
template path = do
    tpl <- getTemplate path
    (return.toResponse.toString) tpl

templateWith :: FilePath -> TemplateMonad a -> AppServerPartT Response
templateWith path f = do
    -- let fullPath = joinPath [templateDir, path]
    -- content <- liftIO $ readFile fullPath
    tpl <- getTemplate path
    let finalTpl = execState f tpl
    (return.toResponse.toString) finalTpl

assign ::  (ToSElem a) => String -> a -> TemplateMonad ()
assign key value = do
    tpl <- get
    let newTpl = setAttribute key value tpl
    put newTpl








