-- | Provides a monad transformer for working with app specific state within a 'Happstack.Server.SimpleHTTP.ServerPartT'.
-- Also provides associated functions to easilly handle various related tasks.
module Framework.AppState (
  -- * Exported modules
  module Framework.AppState.Types
, module Framework.AppState.Templates
, module Framework.AppState.Database
  -- * AppState creation and support
, initApp
, runApp
  -- * Helpers
, asHtml
) where

import Happstack.Server.SimpleHTTP (setHeaderM)
import Control.Monad.Reader (runReaderT)
import Text.StringTemplate (STGroup)

import Framework.AppState.Types
import Framework.AppState.Templates
import Framework.AppState.Database

-- | Create a new 'App' instance with default settings and given directory group.
initApp ::  STGroup String -> App
initApp templateDirGroup = do
    App {
      appName = "Series Db"
    , appDatabase = Nothing
    , templateGroup = templateDirGroup
    }

-- | Run a new AppState using 'initApp' to create it.
--
-- Can be used in the following way:
--
-- > let conf = nullConf { port = 8080 }
-- > simpleHTTP' (runApp) conf paths
runApp ::  AppState a -> IO a
runApp appState = do
    dirGroup <- getDirectoryGroup
    flip runReaderT (initApp dirGroup) $ appState

--
-- Helpers
--

-- | Set the output Content-Type to text/html.
asHtml :: AppServerPartT ()
asHtml = setHeaderM "Content-type" "text/html"









