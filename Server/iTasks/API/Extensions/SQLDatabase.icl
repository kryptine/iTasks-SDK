implementation module iTasks.API.Extensions.SQLDatabase

import iTasks, Database.SQL, Database.SQL.MySQL, Data.Error
import iTasks.Framework.IWorld, iTasks.Framework.Shared

derive class iTask SQLValue, SQLDate, SQLTime


sqlShare :: SQLDatabase String (A.*cur: *cur -> *(MaybeErrorString r,*cur) | SQLCursor cur)
								(A.*cur: w *cur -> *(MaybeErrorString Void, *cur) | SQLCursor cur) -> ReadWriteShared r w 
sqlShare db name readFun writeFun = createChangeOnWriteSDS "SQLShares" name read write
where
	read iworld=:{IWorld|world}
		# (mbOpen,world) = openMySQLDb db world
		= case mbOpen of
			Error e			= (Error e,  {IWorld|iworld & world = world})
			Ok (cur,con,cxt)
				# (res,cur) = readFun cur
				# world				= closeMySQLDb cur con cxt world
				= (res,{IWorld|iworld & world = world})
	write w iworld=:{IWorld|world}
		# (mbOpen,world) = openMySQLDb db world
		= case mbOpen of
			Error e			= (Error e,  {IWorld|iworld & world = world})
			Ok (cur,con,cxt)
				# (res,cur) = writeFun w cur
				# world		= closeMySQLDb cur con cxt world
				= (res,{IWorld|iworld & world = world})

sqlExecute :: SQLDatabase (A.*cur: *cur -> *(MaybeErrorString a,*cur) | SQLCursor cur) -> Task a | iTask a
sqlExecute db queryFun = mkInstantTask exec
where
	exec _ iworld=:{IWorld|world}
		# (mbOpen,world)	= openMySQLDb db world
		= case mbOpen of
			Error e			= (Error (dynamic e,toString e),  {IWorld|iworld & world = world})
			Ok (cur,con,cxt)
				# (res,cur)		= queryFun cur
				# world			= closeMySQLDb cur con cxt world
				= case res of
					Error e		= (Error (dynamic e,toString e),  {IWorld|iworld & world = world}) 
					Ok v		= (Ok v,{IWorld|iworld & world = world})

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

execDelete :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString Void,*cur) | SQLCursor cur
execDelete query values cur
    # (err,cur) 		= execute query values cur
	| isJust err		= (Error (toString (fromJust err)),cur)
	= (Ok Void,cur)

sqlExecuteSelect :: SQLDatabase SQLStatement ![SQLValue] -> Task [SQLRow]
sqlExecuteSelect db query values = sqlExecute db (execSelect query values) 

sqlSelectShare :: SQLDatabase SQLStatement ![SQLValue] -> ReadOnlyShared [SQLRow] 
sqlSelectShare db query values = createReadOnlySDSError read
where
	read iworld=:{IWorld|world}
		# (mbOpen,world) = openMySQLDb db world
		= case mbOpen of
			Error e			= (Error e,  {IWorld|iworld & world = world})
			Ok (cur,con,cxt)
				# (err,cur)			= execute query values cur
				| isJust err		= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
				# (err,rows,cur)	= fetchAll cur
				| isJust err		= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
				# world				= closeMySQLDb cur con cxt world
				= (Ok rows,{IWorld|iworld & world = world})
		
openMySQLDb :: !SQLDatabase !*World -> (MaybeErrorString (!*MySQLCursor, !*MySQLConnection, !*MySQLContext), !*World)
openMySQLDb db world
	# (err,mbContext,world) 	= openContext world
	| isJust err				= (Error (toString (fromJust err)),world)
	# (err,mbConn,context)		= openConnection db (fromJust mbContext)
	| isJust err				= (Error (toString (fromJust err)),world)
	# (err,mbCursor,connection)	= openCursor (fromJust mbConn)
	| isJust err				= (Error (toString (fromJust err)),world)
	= (Ok (fromJust mbCursor,connection, context), world)
				
closeMySQLDb :: !*MySQLCursor !*MySQLConnection !*MySQLContext !*World -> *World
closeMySQLDb cursor connection context world
	# (err,connection)	= closeCursor cursor connection
	# (err,context) 	= closeConnection connection context
	# (err,world)		= closeContext context world
	= world
