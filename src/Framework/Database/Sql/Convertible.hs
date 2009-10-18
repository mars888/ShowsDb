{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Framework.Database.Sql.Convertible where

import Framework.Database.Sql.Types

-- * Convertible items.

-- | Convert value to a field.
class FieldConvertible a where
    toField :: a -> Field
instance FieldConvertible Field  where toField = id
instance FieldConvertible String where toField = Field
                                       --
-- | Convert value into table construct.
class TableConvertible a where
    toTable :: a -> Table
instance TableConvertible String where toTable = Table
instance TableConvertible Table  where toTable = id

-- | Convert value into something usable in a from clause.
class FromConvertible a where
    toFrom :: a -> [Table]
instance FromConvertible String   where toFrom = (:[]).Table
instance FromConvertible Table    where toFrom = (:[])
instance FromConvertible [String] where toFrom = map Table
instance FromConvertible [Table]  where toFrom = id

class UpdateConvertible a where
    toUpdate :: a -> [FieldUpdate]
instance UpdateConvertible String        where toUpdate up = [RawUpdate up []]
instance UpdateConvertible FieldUpdate   where toUpdate    = (:[])
instance UpdateConvertible [FieldUpdate] where toUpdate    = id

class OrderByConvertible a where
    toOrderBy :: a -> [Field]
instance OrderByConvertible String   where toOrderBy = (:[]).toField
instance OrderByConvertible Field    where toOrderBy = (:[])
instance OrderByConvertible [String] where toOrderBy = map toField

