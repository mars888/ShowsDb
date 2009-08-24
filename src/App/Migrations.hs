module App.Migrations (
	createTables
) where

import Database.HDBC

createSeriesTable :: IConnection c => c -> IO ()
createSeriesTable db = do
    let query = "CREATE TABLE IF NOT EXISTS tvshows ("    ++ 
                "  id INTEGER PRIMARY KEY AUTOINCREMENT," ++ 
                "  name VARCHAR(255) DEFAULT \"\","       ++ 
                "  description TEXT DEFAULT \"\","        ++ 
                "  url VARCHAR(255) DEFAULT \"\""         ++ 
                ")"
    stmnt <- prepare db query
    execute stmnt []
    commit db
    putStrLn "Initialized table `tvshows'"

createTables ::  (IConnection c) => c -> IO ()
createTables db = do
	createSeriesTable db
