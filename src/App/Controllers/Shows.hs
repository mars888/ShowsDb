module App.Controllers.Shows (
  paths
) where

import Control.Monad
import Control.Monad.Trans(liftIO)
import Happstack.Server.SimpleHTTP

import Framework.AppState
import Framework.Database.Model
import qualified App.Models.TVShow as TVShow

paths ::  AppServerPartT Response
paths = msum [
      dir "new" new
    , methodSP POST create
    , methodSP GET  index
    ]

index ::  AppServerPartT Response
index = do
    asHtml
    -- shows <- fetchAllModels "SELECT * FROM tvshows ORDER BY name" []
    shows <- fetchAllModels $ select "*" `from` "tvshows" `orderBy` "name"
    templateWith "shows_index" (assign "shows" (shows::[TVShow.TVShow]))

new ::  AppServerPartT Response
new = do
    asHtml
    template "shows_new"

showFromRequest = do
    name        <- look "name"
    description <- look "description"
    url         <- look "url"
    return $ TVShow.TVShow { TVShow.id = -1
                           , TVShow.name = name
                           , TVShow.description = description
                           , TVShow.url = url
                           }

instance FromData TVShow.TVShow where
    fromData = showFromRequest

create ::  AppServerPartT Response
create = do
    asHtml
    tvshow <- getData
    case tvshow of
         Nothing         -> (return.toResponse) "Invalid"
         Just tvshow -> do
             --execute $ insertInto "tvshows" `insertValues` ["name"        #># TVShow.name        tvshow
                                                           --,"description" #># TVShow.description tvshow
                                                           --,"url"         #># TVShow.url         tvshow
                                                           --]
             execute $ insertInto (tvshow :: TVShow.TVShow)
             commit
             redirectTo "/shows/"








