module App.Controllers.Shows (
  paths
) where

import Control.Monad
import Control.Monad.Trans(liftIO)
import Happstack.Server.SimpleHTTP
import Database.HDBC

import Framework.AppState
import Framework.Database.Model
import qualified App.Models.TVShow as TVShow

paths ::  AppServerPartT Response
paths = msum [
      dir "new" new
    , index
    ]

index ::  AppServerPartT Response
index = do
    asHtml
    db <- getDatabase
    shows <- liftIO $ do
        stmnt <- prepare db "SELECT * FROM tvshows ORDER BY name"
        execute stmnt []
        rows <- fetchAllRowsAL' stmnt
        return $ (fromRows rows :: [TVShow.TVShow])
    templateWith "shows_index" (assign "shows" shows)

new ::  AppServerPartT Response
new = do
    asHtml
    template "shows_new"







