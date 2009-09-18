module Framework.Database.Specification (
  Field(..)
, Table(..)
, defineTable
, fieldNames
) where

import Data.List(intercalate)

-- | Name of a field.
type FieldName = String

-- | Defines a field in a database table.
data Field =
      PrimaryKeyField { fieldName :: FieldName }
    | IntegerField    { fieldName :: FieldName }
    | StringField     { fieldName :: FieldName }
    | TextField       { fieldName :: FieldName }
    deriving(Show, Read)


-- | Name of a table.
type TableName = String

-- | Defines a table in a database with associated fields.
data Table = Table
    { tableName :: TableName -- ^ Name of the table in the database.
    , fields    :: [Field]
    }
    deriving(Show, Read)

-- | Build a table definition.
defineTable ::  TableName -- ^ Name of the table.
            -> [Field]    -- ^ Fields that make up the table.
            -> Table
defineTable name fields = Table name fields

-- | Produce a list of the names of all fields in a table.
fieldNameList :: Table -> [FieldName]
fieldNameList = map fieldName . fields

-- | Produce a string list of the names of all fields in a table.
--
-- > -- Example:
-- > table = defineTable "mytable" [
-- >          IntegerField "id"
-- >        , StringField "name"
-- >        ]
-- > fieldNames table -- Produces: "id, name"
fieldNames :: Table -> String
fieldNames = intercalate ", " . fieldNameList

