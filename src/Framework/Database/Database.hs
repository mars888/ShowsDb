module Framework.Database.Database (
    connectDatabase
)where

import Database.HDBC
import Database.HDBC.Sqlite3

databasePath ::  FilePath
databasePath = "database.db"

connectDatabase ::  IO Connection
connectDatabase = connectSqlite3 databasePath

withDatabase f = do
	db <- connectDatabase
	res <- f db
	disconnect db
	return res

