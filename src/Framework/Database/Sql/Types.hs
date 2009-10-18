module Framework.Database.Sql.Types where

import Database.HDBC (SqlValue)

type FieldName = String
type TableName = String
type LiteralValue = SqlValue

data Field =
      Field FieldName
    | FieldTable TableName FieldName
    | FieldAlias Field FieldName
    deriving (Show)

data FieldUpdate = FieldUpdate Field SqlValue
                 | RawUpdate String [SqlValue]
    deriving (Show)

data Table =
      Table TableName
    | TableAlias TableName TableName
    deriving (Show)

data WhereClause =
      WhereEquals Field LiteralValue
    | WhereAnd WhereClause WhereClause
    | WhereOr WhereClause WhereClause
    | WhereRaw String
    deriving (Show)

data SelectStatement = SelectStatement {
      selectFields  :: [Field]
    , selectFrom    :: [Table]
    , selectWhere   :: Maybe WhereClause
    , selectOrderBy :: Maybe [Field]
    , selectLimit   :: Maybe String
    }
    deriving (Show)

data UpdateStatement = UpdateStatement {
      updateTable  :: Table
    , updateValues :: [FieldUpdate]
    , updateWhere  :: Maybe WhereClause
    }
    deriving (Show)

data DeleteStatement = DeleteStatement {
      deleteTable :: Table
    , deleteWhere :: Maybe WhereClause
    }
    deriving (Show)

data InsertStatement = InsertStatement {
      insertTable :: Table
    , insertItems :: [FieldUpdate]
    }
    deriving (Show)

emptySelect :: SelectStatement
emptySelect = SelectStatement {
      selectFields  = []
    , selectFrom    = []
    , selectWhere   = Nothing
    , selectOrderBy = Nothing
    , selectLimit   = Nothing
    }

emptyUpdate :: UpdateStatement
emptyUpdate = UpdateStatement {
      updateTable  = Table ""
    , updateValues = []
    , updateWhere  = Nothing
    }

emptyDelete :: DeleteStatement
emptyDelete = DeleteStatement {
      deleteTable = Table ""
    , deleteWhere = Nothing
    }

emptyInsert :: InsertStatement
emptyInsert = InsertStatement {
      insertTable = Table ""
    , insertItems = []
    }







