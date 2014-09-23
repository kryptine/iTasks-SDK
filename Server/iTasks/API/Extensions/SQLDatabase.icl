implementation module iTasks.API.Extensions.SQLDatabase

import iTasks, Database.SQL, Database.SQL.MySQL, Database.SQL.SQLite, Data.Error, Data.Func, System.FilePath
import iTasks.Framework.IWorld, iTasks.Framework.SDS
from iTasks.Framework.SDS import reportSDSChange
import qualified Data.Map

//Extend Resource type for mysql resources
:: *Resource | MySQLResource *(!*MySQLCursor, !*MySQLConnection, !*MySQLContext)
             | SQLiteResource *(!*SQLiteCursor, !*SQLiteConnection, !*SQLiteContext)

derive class iTask SQLDatabaseDef, SQLDatabase, SQLValue, SQLTime, SQLDate, SQLTable, SQLColumn, SQLColumnType

sqlShare :: String (A.*cur: p *cur -> *(MaybeErrorString r,*cur) | SQLCursor cur)
                   (A.*cur: p w *cur -> *(MaybeErrorString (), *cur) | SQLCursor cur) -> RWShared (SQLDatabaseDef,p) r w
sqlShare name readFun writeFun = createReadWriteSDS "SQLShares" name (readFunSQL readFun) (writeFunSQL writeFun)

readFunSQL :: (A.*cur: p *cur -> *(MaybeErrorString r,*cur) | SQLCursor cur) (SQLDatabaseDef,p) *IWorld -> (!MaybeError TaskException r,!*IWorld)
readFunSQL fun (MySQLDatabase db,p) iworld
    # (mbOpen,iworld) = openMySQLDB db iworld
	= case mbOpen of
	    Error e			= (Error (exception e),  iworld)
		Ok (cur,con,cxt)
		    # (res,cur) = fun p cur
			# iworld	= closeMySQLDB cur con cxt iworld
            = case res of
                (Ok v)      = (Ok v,iworld)
                (Error e)   = (Error (exception e),iworld)

readFunSQL fun (SQLiteDatabase path,p) iworld
    # db = {SQLDatabase|database=path,host=Nothing,username=Nothing,password=Nothing}
    # (mbOpen,iworld) = openSQLiteDB db iworld
	= case mbOpen of
	    Error e			= (Error (exception e),  iworld)
		Ok (cur,con,cxt)
		    # (res,cur) = fun p cur
			# iworld	= closeSQLiteDB cur con cxt iworld
            = case res of
                (Ok v)      = (Ok v,iworld)
                (Error e)   = (Error (exception e),iworld)

writeFunSQL :: (A.*cur: p w *cur -> *(MaybeErrorString (), *cur) | SQLCursor cur) (SQLDatabaseDef,p) w *IWorld -> (!MaybeError TaskException (SDSNotifyPred (SQLDatabaseDef,p)),!*IWorld)
writeFunSQL fun (MySQLDatabase db,p) w iworld
    # (mbOpen,iworld) = openMySQLDB db iworld
	= case mbOpen of
	    Error e			= (Error (exception e), iworld)
		Ok (cur,con,cxt)
		    # (res,cur) = fun p w cur
			# iworld	= closeMySQLDB cur con cxt iworld
            = case res of
                (Ok _)      = (Ok (const True),iworld)
                (Error e)   = (Error (exception e),iworld)

writeFunSQL fun (SQLiteDatabase path,p) w iworld
    # db = {SQLDatabase|database=path,host=Nothing,username=Nothing,password=Nothing}
    # (mbOpen,iworld) = openSQLiteDB db iworld
	= case mbOpen of
	    Error e			= (Error (exception e), iworld)
		Ok (cur,con,cxt)
		    # (res,cur) = fun p w cur
			# iworld	= closeSQLiteDB cur con cxt iworld
            = case res of
                (Ok _)      = (Ok (const True),iworld)
                (Error e)   = (Error (exception e),iworld)
	
sqlExecute :: SQLDatabaseDef [String] (A.*cur: *cur -> *(MaybeErrorString a,*cur) | SQLCursor cur) -> Task a | iTask a
sqlExecute (MySQLDatabase db) touchIds queryFun = mkInstantTask eval
where
	eval _ iworld
		# (mbOpen,iworld)	= openMySQLDB db iworld
		= case mbOpen of
			Error e			= (Error (exception e), iworld)
			Ok (cur,con,cxt)
				# (res,cur)		= queryFun cur
				# iworld		= closeMySQLDB cur con cxt iworld
				= case res of
					Error e		= (Error (exception e), iworld)
					Ok v		
                        //Trigger share change for all touched ids
                        # iworld = seqSt (\s w -> reportSDSChange ("SQLShares:"+++s) w) touchIds iworld
                        = (Ok v,iworld)
sqlExecute (SQLiteDatabase path) touchIds queryFun = mkInstantTask eval
where
	eval _ iworld
        # db = {SQLDatabase|database=path,host=Nothing,username=Nothing,password=Nothing}
		# (mbOpen,iworld)	= openSQLiteDB db iworld
		= case mbOpen of
			Error e			= (Error (exception e), iworld)
			Ok (cur,con,cxt)
				# (res,cur)		= queryFun cur
				# iworld		= closeSQLiteDB cur con cxt iworld
				= case res of
					Error e		= (Error (exception e), iworld)
					Ok v		
                        //Trigger share change for all touched ids
                        # iworld = seqSt (\s w -> reportSDSChange ("SQLShares:"+++s) w) touchIds iworld
                        = (Ok v,iworld)

execSelect :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString [SQLRow],*cur) | SQLCursor cur
execSelect query values cur
	# (err,cur)			= execute query values cur
	| isJust err		= (Error (toString (fromJust err)),cur)
	# (err,rows,cur)	= fetchAll cur
	| isJust err		= (Error (toString (fromJust err)),cur)
	= (Ok rows,cur)

execInsert :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString Int,*cur) | SQLCursor cur
execInsert query values cur
    # (err,cur) 		= execute query values cur
	| isJust err		= (Error (toString (fromJust err)),cur)
	# (err,id,cur)		= insertId cur
	| isJust err		= (Error (toString (fromJust err)),cur)
	= (Ok id,cur)

execDelete :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString (),*cur) | SQLCursor cur
execDelete query values cur
    # (err,cur) 		= execute query values cur
	| isJust err		= (Error (toString (fromJust err)),cur)
	= (Ok (),cur)

sqlExecuteSelect :: SQLDatabaseDef SQLStatement ![SQLValue] -> Task [SQLRow]
sqlExecuteSelect db query values = sqlExecute db [] (execSelect query values)

sqlSelectShare :: String SQLStatement ![SQLValue] -> ROShared SQLDatabaseDef [SQLRow]
sqlSelectShare name query values = sdsTranslate "sqlSelectShare" (\db -> (db,())) (createReadWriteSDS "SQLShares" name (readFunSQL readFun) write)
where
    readFun () cur
        # (err,cur)			= execute query values cur
        | isJust err		= (Error (toString (fromJust err)),cur)
		# (err,rows,cur)	= fetchAll cur
		| isJust err		= (Error (toString (fromJust err)),cur)
        = (Ok rows,cur)
    write _ () iworld = (Ok (const True),iworld)
		
sqlTables :: ROShared SQLDatabaseDef [SQLTableName]
sqlTables = createReadOnlySDSError read
where
    read (MySQLDatabase db) iworld
		# (mbOpen,iworld) = openMySQLDB db iworld
		= case mbOpen of
			Error e			= (Error (exception e), iworld)
			Ok (cur,con,cxt)
                # (err,tables,cur)  = listTables cur
				| isJust err		= (Error (exception (toString (fromJust err))),iworld)
				# iworld            = closeMySQLDB cur con cxt iworld
				= (Ok tables,iworld)
    read (SQLiteDatabase path) iworld
        # db = {SQLDatabase|database=path,host=Nothing,username=Nothing,password=Nothing}
		# (mbOpen,iworld) = openSQLiteDB db iworld
		= case mbOpen of
			Error e			= (Error (exception e), iworld)
			Ok (cur,con,cxt)
                # (err,tables,cur)  = listTables cur
				| isJust err		= (Error (exception (toString (fromJust err))),iworld)
				# iworld            = closeSQLiteDB cur con cxt iworld
				= (Ok tables,iworld)

sqlTableDefinition :: ROShared (SQLDatabaseDef,SQLTableName) SQLTable
sqlTableDefinition = createReadOnlySDSError read
where
    read (MySQLDatabase db,tablename) iworld
		# (mbOpen,iworld) = openMySQLDB db iworld
		= case mbOpen of
			Error e			= (Error (exception e), iworld)
			Ok (cur,con,cxt)
                # (err,mbTable,cur) = describeTable tablename cur
				| isJust err		= (Error (exception (toString (fromJust err))),iworld)
				# iworld            = closeMySQLDB cur con cxt iworld
				= (Ok (fromJust mbTable),iworld)

    read (SQLiteDatabase path,tablename) iworld
        # db = {SQLDatabase|database=path,host=Nothing,username=Nothing,password=Nothing}
		# (mbOpen,iworld) = openSQLiteDB db iworld
		= case mbOpen of
			Error e			= (Error (exception e), iworld)
			Ok (cur,con,cxt)
                # (err,mbTable,cur) = describeTable tablename cur
				| isJust err		= (Error (exception (toString (fromJust err))),iworld)
				# iworld            = closeSQLiteDB cur con cxt iworld
				= (Ok (fromJust mbTable),iworld)

sqlExecuteCreateTable :: SQLDatabaseDef SQLTable -> Task ()
sqlExecuteCreateTable (MySQLDatabase db) table = mkInstantTask eval
where
	eval _ iworld
		# (mbOpen,iworld)	= openMySQLDB db iworld
		= case mbOpen of
			Error e			= (Error (dynamic e,toString e), iworld)
			Ok (cur,con,cxt)
				# (res,cur)		= createTable table cur
				# iworld		= closeMySQLDB cur con cxt iworld
				= case res of
					Just e		= (Error (dynamic e,toString e), iworld)
					Nothing     = (Ok (), iworld)
sqlExecuteCreateTable (SQLiteDatabase path) table = mkInstantTask eval
where
	eval _ iworld
        # db = {SQLDatabase|database=path,host=Nothing,username=Nothing,password=Nothing}
		# (mbOpen,iworld)	= openSQLiteDB db iworld
		= case mbOpen of
			Error e			= (Error (dynamic e,toString e), iworld)
			Ok (cur,con,cxt)
				# (res,cur)		= createTable table cur
				# iworld		= closeSQLiteDB cur con cxt iworld
				= case res of
					Just e		= (Error (dynamic e,toString e), iworld)
					Nothing     = (Ok (), iworld)

sqlExecuteDropTable :: SQLDatabaseDef SQLTableName -> Task ()
sqlExecuteDropTable (MySQLDatabase db) tablename = mkInstantTask eval
where
	eval _ iworld
		# (mbOpen,iworld)	= openMySQLDB db iworld
		= case mbOpen of
			Error e			= (Error (dynamic e,toString e), iworld)
			Ok (cur,con,cxt)
				# (res,cur)		= deleteTable tablename cur
				# iworld		= closeMySQLDB cur con cxt iworld
				= case res of
					Just e		= (Error (dynamic e,toString e), iworld)
					Nothing     = (Ok (), iworld)
sqlExecuteDropTable (SQLiteDatabase path) tablename = mkInstantTask eval
where
	eval _ iworld
        # db = {SQLDatabase|database=path,host=Nothing,username=Nothing,password=Nothing}
		# (mbOpen,iworld)	= openSQLiteDB db iworld
		= case mbOpen of
			Error e			= (Error (dynamic e,toString e), iworld)
			Ok (cur,con,cxt)
				# (res,cur)		= deleteTable tablename cur
				# iworld		= closeSQLiteDB cur con cxt iworld
				= case res of
					Just e		= (Error (dynamic e,toString e), iworld)
					Nothing     = (Ok (), iworld)

openMySQLDB :: !SQLDatabase !*IWorld -> (MaybeErrorString (!*MySQLCursor, !*MySQLConnection, !*MySQLContext), !*IWorld)
openMySQLDB db iworld=:{IWorld|resources=Just (MySQLResource con)}
    = (Ok con, {IWorld|iworld & resources=Nothing})
openMySQLDB db iworld=:{IWorld|resources=Nothing}
            # iworld=:{IWorld|world} = {IWorld|iworld & resources = Nothing}
        	# (err,mbContext,world) 	= openContext world
        	| isJust err				= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
        	# (err,mbConn,context)		= openConnection db (fromJust mbContext)
        	| isJust err				= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
        	# (err,mbCursor,connection)	= openCursor (fromJust mbConn)
        	| isJust err				= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
        	= (Ok (fromJust mbCursor,connection, context),{IWorld|iworld & world = world})
openMySQLDB db iworld = openMySQLDB db (closeCurrentResource iworld)

closeMySQLDB :: !*MySQLCursor !*MySQLConnection !*MySQLContext !*IWorld -> *IWorld
closeMySQLDB cursor connection context iworld=:{IWorld|resources=Nothing}
   = {IWorld|closeCurrentResource iworld & resources=Just (MySQLResource (cursor,connection,context))}

openSQLiteDB :: !SQLDatabase !*IWorld -> (MaybeErrorString (!*SQLiteCursor, !*SQLiteConnection, !*SQLiteContext), !*IWorld)
openSQLiteDB db iworld=:{IWorld|resources=Just (SQLiteResource con)}
    = (Ok con, {IWorld|iworld & resources=Nothing})
openSQLiteDB db iworld=:{IWorld|resources=Nothing}
    # iworld=:{IWorld|world,server={paths={dataDirectory}}} = {IWorld|iworld & resources = Nothing}
    # db = {db & database = dataDirectory </> db.database}
    # (err,mbContext,world) 	= openContext world
    | isJust err				= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
    # (err,mbConn,context)		= openConnection db (fromJust mbContext)
    | isJust err				= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
    # (err,mbCursor,connection)	= openCursor (fromJust mbConn)
    | isJust err				= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
    = (Ok (fromJust mbCursor,connection, context),{IWorld|iworld & world = world})
openSQLiteDB db iworld = openSQLiteDB db (closeCurrentResource iworld)

closeSQLiteDB :: !*SQLiteCursor !*SQLiteConnection !*SQLiteContext !*IWorld -> *IWorld
closeSQLiteDB cursor connection context iworld=:{IWorld|resources=Nothing}
   = {IWorld|closeCurrentResource iworld & resources=Just (SQLiteResource (cursor,connection,context))}

closeCurrentResource :: !*IWorld -> *IWorld
closeCurrentResource iworld=:{IWorld|resources=Nothing} = iworld
closeCurrentResource iworld=:{IWorld|resources=Just (SQLiteResource (cursor,connection,context)),world}
	# (err,connection)	= closeCursor cursor connection
	# (err,context) 	= closeConnection connection context
	# (err,world)		= closeContext context world
	= {IWorld|iworld & resources=Nothing, world = world}
closeCurrentResource iworld=:{IWorld|resources=Just (MySQLResource (cursor,connection,context)),world}
	# (err,connection)	= closeCursor cursor connection
	# (err,context) 	= closeConnection connection context
	# (err,world)		= closeContext context world
	= {IWorld|iworld & resources=Nothing, world = world}

