{-# LANGUAGE DeriveDataTypeable #-}
module App.Models.TVShow where

import Prelude hiding (id)

import Database.HDBC
import Data.Typeable(Typeable)
import Data.Data(Data)
import Text.StringTemplate.GenericStandard

import Database.HDBC
import Framework.Database.Model
import Framework.Database.Specification
import Framework.Database.Sql

data TVShow = TVShow {
      id          :: Int
    , name        :: String
    , description :: String
    , url         :: String
    }
    deriving(Show, Read, Typeable, Data)

table = defineTable "tvshows" [
      PrimaryKeyField "id"
    , StringField     "name"
    , StringField     "url"
    , TextField       "description"
    ]

instance DBModel TVShow where
    fromRow lst = TVShow
                    (justLook lst "id")
                    (justLook lst "name")
                    (justLook lst "description")
                    (justLook lst "url")
    toRow       = columnsToRow
                    [ ("id",            toSql.id)
                    , ("name",          toSql.name)
                    , ("description",   toSql.description)
                    , ("url",           toSql.url)
                    ]


empty ::  TVShow
empty = TVShow { id             = -1
             , name           = "N/A"
             , description    = "N/A"
             , url            = ""
             }


instance Insertable TVShow where
    insertInto show = insertInto "tvshows" `insertValues`
                                           [ "name"        #># name show
                                           , "url"         #># url show
                                           , "description" #># description show
                                           ]




