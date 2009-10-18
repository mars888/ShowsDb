module Framework.Database.Sql.Generation where

import Data.List (intercalate, foldl')
import Database.HDBC (SqlValue)

import Framework.Database.Sql.Types

class GenerateQuery a where
    toQuery :: a -> String
class GenerateParams a where
    toParams :: a -> [SqlValue]

instance GenerateQuery SelectStatement where toQuery = selectToQuery
instance GenerateQuery UpdateStatement where toQuery = updateToQuery
instance GenerateQuery DeleteStatement where toQuery = deleteToQuery
instance GenerateQuery InsertStatement where toQuery = insertToQuery

instance GenerateParams SelectStatement where toParams = selectToParams
instance GenerateParams UpdateStatement where toParams = updateToParams
instance GenerateParams DeleteStatement where toParams = deleteToParams
instance GenerateParams InsertStatement where toParams = insertToParams

-- * Helpers

fieldToString :: Field -> String
fieldToString (Field name)             = name
fieldToString (FieldTable table name)  = table ++ "." ++ name
fieldToString (FieldAlias field alias) = (fieldToString field) ++ " as " ++ alias

fieldList :: [Field] -> String
fieldList = intercalate ", " . map fieldToString

tableToString :: Table -> String
tableToString (Table name)            = name
tableToString (TableAlias name alias) = name ++ " AS " ++ alias

fromList :: [Table] -> String
fromList = intercalate ", " . map tableToString

generateWhere :: WhereClause -> String
generateWhere (WhereEquals field val) = fieldToString field ++ " = ?"
generateWhere (WhereAnd left right)   = "(" ++ (generateWhere left) ++ " AND " ++ (generateWhere right) ++ ")"
generateWhere (WhereOr left right)    = "(" ++ (generateWhere left) ++ " OR "  ++ (generateWhere right) ++ ")"
generateWhere (WhereRaw str)          = str

generateOrderBy :: [Field] -> String
generateOrderBy = fieldList

whereToParams :: WhereClause -> [SqlValue]
whereToParams (WhereEquals _ val) = [val]
whereToParams (WhereAnd left right) = whereToParams left ++ whereToParams right
whereToParams (WhereOr  left right) = whereToParams left ++ whereToParams right

generateSetPart :: [FieldUpdate] -> String
generateSetPart = intercalate ", " . map makePart
    where makePart (FieldUpdate field _) = fieldToString field ++ " = ?"
          makePart (RawUpdate val _)     = val

setPartToParams :: [FieldUpdate] -> [SqlValue]
setPartToParams = foldl' (++) [] . map raise
    where raise (FieldUpdate _ vals) = [vals]
          raise (RawUpdate _ vals)   = vals

-- * Generators

selectToQuery :: SelectStatement -> String
selectToQuery sel = "SELECT " ++ fields ++ " FROM " ++ from ++ whereClause ++ orderBy -- TODO LIMIT
    where fields = fieldList . selectFields $ sel
          from   = fromList  . selectFrom   $ sel
          whereClause = case selectWhere sel of
                         Nothing -> ""
                         Just w  -> " WHERE " ++ generateWhere w
          orderBy = case selectOrderBy sel of
                         Nothing -> ""
                         Just obFields -> " ORDER BY " ++ generateOrderBy obFields

fieldListFromUpdateList :: [FieldUpdate] -> String
fieldListFromUpdateList = intercalate ", " . map updateToString
    where updateToString (FieldUpdate field _) = fieldToString field
          updateToString (RawUpdate _ _)       = error "Can't use raw sql in insert query."

-- | Extract parameters from select query.
selectToParams :: SelectStatement -> [SqlValue]
selectToParams sel = case selectWhere sel of
                          Nothing -> []
                          Just w  -> whereToParams w


updateToQuery :: UpdateStatement -> String
updateToQuery up = "UPDATE " ++ table ++ " SET " ++ setPart ++ whereClause
    where table   = tableToString . updateTable  $ up
          setPart = generateSetPart . updateValues $ up
          whereClause = case updateWhere up of
                             Nothing -> ""
                             Just w  -> " WHERE " ++ generateWhere w

updateToParams :: UpdateStatement -> [SqlValue]
updateToParams up = setPartValues ++ whereClauseValues
    where setPartValues = setPartToParams . updateValues $ up
          whereClauseValues = case updateWhere up of
                                   Nothing -> []
                                   Just w  -> whereToParams w


deleteToQuery :: DeleteStatement -> String
deleteToQuery del = "DELETE FROM " ++ table ++ whereClause
    where table = tableToString . deleteTable $ del
          whereClause = case deleteWhere del of
                             Nothing -> []
                             Just w  -> " WHERE " ++ generateWhere w

deleteToParams :: DeleteStatement -> [SqlValue]
deleteToParams del = case deleteWhere del of
                          Nothing -> []
                          Just w  -> whereToParams w


insertToQuery :: InsertStatement -> String
insertToQuery ins = "INSERT INTO " ++ table ++ "(" ++ fields ++ ")" ++ " VALUES (" ++ values ++ ")"
    where table = tableToString . insertTable $ ins
          fields = fieldListFromUpdateList . insertItems $ ins
          values = intercalate ", " $ take (length.insertItems$ins) (repeat "?")

insertToParams :: InsertStatement -> [SqlValue]
insertToParams = setPartToParams . insertItems










