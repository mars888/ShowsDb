{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Framework.Database.Sql.Operations where

import Database.HDBC (SqlValue, toSql)

import Framework.Database.Sql.Types
import Framework.Database.Sql.Convertible
import qualified Data.Convertible as C


-- | Applies to classes which can be modified by a 'WhereClause'.
class WhereModifiable a where
    whereIs :: a -> WhereClause -> a
instance WhereModifiable SelectStatement where
    whereIs q w = q { selectWhere = Just w }
instance WhereModifiable UpdateStatement where
    whereIs u w = u { updateWhere = Just w }
instance WhereModifiable DeleteStatement where
    whereIs d w = d { deleteWhere = Just w }


-- * Query creation.

-- | Create select query.
class Selectable a where
    select :: a -> SelectStatement
instance Selectable String where
    select str = emptySelect {
          selectFields = [Field str]
        }
instance Selectable Field where
    select field = emptySelect {
          selectFields = [field]
        }
instance Selectable [String] where
    select strs = emptySelect {
          selectFields = map Field strs
        }

-- | Create update query.
class Updatable a b where
    update :: a -> b -> UpdateStatement
instance (TableConvertible a) => Updatable a [FieldUpdate] where
    update tab fields = emptyUpdate {
          updateTable  = toTable tab
        , updateValues = fields
        }

-- | Create delete query.
class Deletable a where
    deleteFrom :: a -> DeleteStatement
instance (TableConvertible a) => Deletable a  where
    deleteFrom tab = emptyDelete { deleteTable = toTable tab }

-- | Create insert query.
class Insertable a where
    insertInto :: a -> InsertStatement
-- | Could posible be
--
-- > instance (TableConvertible a) => Insertable a
--
-- But this causes some issues when trying to override other classes.
instance Insertable String where
    insertInto str = emptyInsert { insertTable = toTable str }

-- * General operators.
from :: (FromConvertible a) => SelectStatement -> a -> SelectStatement
from sel tab = sel { selectFrom = toFrom tab }

orderBy :: (OrderByConvertible a) => SelectStatement -> a -> SelectStatement
orderBy sel oby = sel { selectOrderBy = Just (toOrderBy oby) }

insertValues :: (UpdateConvertible a) => InsertStatement -> a -> InsertStatement
insertValues ins vals = ins { insertItems = toUpdate vals }

-- * Where operators.
(#=#) :: (FieldConvertible a, C.Convertible b SqlValue) => a -> b -> WhereClause
(#=#) field value = WhereEquals (toField field) (toSql value)
andWhere :: WhereClause -> WhereClause -> WhereClause
andWhere = WhereAnd
orWhere :: WhereClause -> WhereClause -> WhereClause
orWhere = WhereOr

-- * Update operators.
-- (#>#) :: (FieldConvertible a, C.Convertible b SqlValue) => a -> b -> FieldUpdate
(#>#) field value = FieldUpdate (toField field) (toSql value)

infixl 2 `whereIs`
infixl 3 `from`
infixl 3 `orderBy`
infixl 4 `orWhere`
infixl 5 `andWhere`
infixl 6 #=#
infixl 7 #>#





