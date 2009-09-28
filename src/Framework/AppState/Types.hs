module Framework.AppState.Types (
  -- * Types
  App(..)
, AppState
, AppServerPartT
  -- * Functions
, getApp
, getAppName
) where

import Happstack.Server.SimpleHTTP (ServerPartT)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, ask, asks)
import Text.StringTemplate (STGroup)

import Framework.Database(CurrentConnection)

-- | General Application state which should be available in handlers.
data App = App {
      appName :: String
    , appDatabase :: Maybe CurrentConnection
    , templateGroup :: STGroup String
    }

-- | Wrap 'App' in a 'ReaderT' to allows for access in a monad.
type AppState = ReaderT App IO
-- | Wrap 'AppState' in a 'ServerPartT' to provide access within a web handler.
type AppServerPartT = ServerPartT AppState

-- | Return the 'App' from within a 'AppServerPartT' monad.
getApp ::  AppServerPartT App
getApp = lift ask

-- | Return the appname of the current 'App' instance.
getAppName ::  AppServerPartT String
getAppName = asks appName
