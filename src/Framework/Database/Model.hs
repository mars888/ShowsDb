{-# LANGUAGE FlexibleContexts #-}
module Framework.Database.Model where

import Database.HDBC
import Data.Convertible
import Data.Maybe(fromJust)

type RowAssocList = [(String, SqlValue)]
type ColumnName = String

class DBModel a where
    -- | Convert single association list row as got from the database via for example `fetchAllRowsAL'
    -- into an instance.
    fromRow ::  RowAssocList -> a

    -- | Convert a list of rows into a list of instances.
    fromRows :: [RowAssocList] -> [a]
    fromRows = map fromRow

    -- | Convert a single instance into a row association list.
    toRow :: a -> RowAssocList

    -- | Convert a list of instances into a list of row associtations.
    toRows :: [a] -> [RowAssocList]
    toRows = map toRow

    -- | Helper function for converting an instance to a row assocation list.
    columnsToRow ::  [(ColumnName, a -> SqlValue)] -> a -> RowAssocList
    columnsToRow cols item = map (\(name, f) -> (name, f item)) cols

-- | Lookup key in association list, convert from maybe (fails if nothing is returned)
-- and converts the resulting Sql value.
justLook :: (Convertible SqlValue a) => RowAssocList -> ColumnName -> a
justLook lst key = fromSql . fromJust $ (flip lookup) lst key


