definition module iTasks.API.Extensions.SQLDatabase
/**
* This experimental extension provides tasks and shares
* for interacting with a relational database.
*
* It provides only mimimal functionality and currently only works with MySQL...
*/
import iTasks, Database.SQL, Data.Error
derive class iTask SQLValue, SQLTime, SQLDate, SQLTable, SQLColumn, SQLColumnType


/**
* Generic SQL Datbase share
* You need to supply the read/write operations as functions using an SQL cursor
*
* @param The database connection details
* @param A unique identifier for the share
* @param The read access function
* @param The write access function
* 
* @return The shared data source
*/
sqlShare :: String (A.*cur: p *cur -> *(MaybeErrorString r,*cur) | SQLCursor cur)
								(A.*cur: p w *cur -> *(MaybeErrorString Void, *cur) | SQLCursor cur) -> RWShared (SQLDatabase,p) r w



/**
* Perform one or multiple queries on an SQL database
*/
sqlExecute	:: SQLDatabase [String] (A.*cur: *cur -> *(MaybeErrorString a,*cur) | SQLCursor cur) -> Task a | iTask a

//Common helper functions for sqlExecute
execSelect :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString [SQLRow],*cur) | SQLCursor cur
execInsert :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString Int,*cur) | SQLCursor cur
execDelete :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString Void,*cur) | SQLCursor cur

/**
* Run a single query and fetch all results
*/
sqlExecuteSelect :: SQLDatabase SQLStatement ![SQLValue] -> Task [SQLRow]

/**
* Read only query that is run each time the share is read.
*
* Note: Although it is possible to do other queries than just selects,
* this is a bad idea. You never know how many times the query will be executed
*/
sqlSelectShare	:: String SQLStatement ![SQLValue] -> ROShared SQLDatabase [SQLRow]

/*
* View the list of tables in a database
*/
sqlTables :: ROShared SQLDatabase [SQLTableName]
/**
* The structure of database table
*/
sqlTableDefinition :: ROShared (SQLDatabase,SQLTableName) SQLTable

sqlExecuteCreateTable :: SQLDatabase SQLTable -> Task Void
sqlExecuteDropTable :: SQLDatabase SQLTableName -> Task Void
