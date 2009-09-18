module Framework.Database (
  CurrentConnection
, connectDatabase
) where

import Database.HDBC
import qualified Database.HDBC.Sqlite3 as Sqlite

data CurrentConnection =
    SqliteConnection Sqlite.Connection

instance Show CurrentConnection where
    show (SqliteConnection _) = "SqliteConnection Sqlite.Connection"

instance IConnection CurrentConnection where
    disconnect           (SqliteConnection c) = disconnect c
    commit               (SqliteConnection c) = commit c
    rollback             (SqliteConnection c) = rollback c
    run                  (SqliteConnection c) = run c
    prepare              (SqliteConnection c) = prepare c
    clone                (SqliteConnection c) = return.SqliteConnection =<< (clone c)
    hdbcDriverName       (SqliteConnection c) = hdbcDriverName c
    hdbcClientVer        (SqliteConnection c) = hdbcClientVer c
    proxiedClientName    (SqliteConnection c) = proxiedClientName c
    proxiedClientVer     (SqliteConnection c) = proxiedClientVer c
    dbServerVer          (SqliteConnection c) = dbServerVer c
    dbTransactionSupport (SqliteConnection c) = dbTransactionSupport c
    getTables            (SqliteConnection c) = getTables c
    describeTable        (SqliteConnection c) = describeTable c



databasePath ::  FilePath
databasePath = "database.db"

connectDatabaseSqlite3 ::  IO Sqlite.Connection
connectDatabaseSqlite3 = Sqlite.connectSqlite3 databasePath

connectDatabase = return.SqliteConnection =<< connectDatabaseSqlite3

withDatabase f = do
	db <- connectDatabase
	res <- f db
	disconnect db
	return res

