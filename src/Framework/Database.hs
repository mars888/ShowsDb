module Framework.Database (
  CurrentConnection
, connectDatabase
) where

import Database.HDBC
import qualified Database.HDBC.Sqlite3 as Sqlite

-- | Wrapper for supporting multiple database types.
-- This implements IConnection to unwrap the current database and
-- call the current IConnection function on the unwrapped value.
data CurrentConnection =
    SqliteConnection Sqlite.Connection

-- | Instance of show for convenience. Note that this isn't quite correct as
-- the result is not syntactically correct Haskell.
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


-- | Database path is hardcoded for now.
databasePath ::  FilePath
databasePath = "database.db"

-- | Connect to default sqlite database.
connectDatabaseSqlite3 ::  IO Sqlite.Connection
connectDatabaseSqlite3 = Sqlite.connectSqlite3 databasePath

-- | Connect to currently required database (which is sqlite only for now).
connectDatabase ::  IO CurrentConnection
connectDatabase = return.SqliteConnection =<< connectDatabaseSqlite3

-- | Call action with default database and disconnects the database afterwards.
withDatabase ::  (CurrentConnection -> IO b) -> IO b
withDatabase f = do
	db <- connectDatabase
	res <- f db
	disconnect db
	return res

