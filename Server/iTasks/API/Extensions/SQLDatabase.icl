implementation module iTasks.API.Extensions.SQLDatabase

import iTasks, Database.SQL, Database.SQL.MySQL, Data.Error, Data.Func
import iTasks.Framework.IWorld, iTasks.Framework.SDS
from iTasks.Framework.SDS import reportSDSChange
import qualified Data.Map

//Extend Resource type for mysql resources
:: *Resource | MySQLResource *(!*MySQLCursor, !*MySQLConnection, !*MySQLContext)

derive class iTask SQLValue, SQLDate, SQLTime

sqlShare :: String (A.*cur: *cur -> *(MaybeErrorString r,*cur) | SQLCursor cur)
								(A.*cur: w *cur -> *(MaybeErrorString Void, *cur) | SQLCursor cur) -> RWShared SQLDatabase r w
sqlShare name readFun writeFun = createReadWriteSDS "SQLShares" name read write
where
	read db iworld
		# (mbOpen,iworld) = openMySQLDb db iworld
		= case mbOpen of
			Error e			= (Error e,  iworld)
			Ok (cur,con,cxt)
				# (res,cur) = readFun cur
				# iworld	= closeMySQLDb cur con cxt iworld
				= (res,iworld)
	write db w iworld
		# (mbOpen,iworld) = openMySQLDb db iworld
		= case mbOpen of
			Error e			= (Error e, iworld)
			Ok (cur,con,cxt)
				# (res,cur) = writeFun w cur
				# iworld	= closeMySQLDb cur con cxt iworld
                = (fmap (const (const True)) res, iworld)

sqlExecute :: SQLDatabase [String] (A.*cur: *cur -> *(MaybeErrorString a,*cur) | SQLCursor cur) -> Task a | iTask a
sqlExecute db touchIds queryFun = mkInstantTask exec
where
	exec _ iworld
		# (mbOpen,iworld)	= openMySQLDb db iworld
		= case mbOpen of
			Error e			= (Error (dynamic e,toString e), iworld)
			Ok (cur,con,cxt)
				# (res,cur)		= queryFun cur
				# iworld		= closeMySQLDb cur con cxt iworld
				= case res of
					Error e		= (Error (dynamic e,toString e), iworld)
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

execDelete :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString Void,*cur) | SQLCursor cur
execDelete query values cur
    # (err,cur) 		= execute query values cur
	| isJust err		= (Error (toString (fromJust err)),cur)
	= (Ok Void,cur)

sqlExecuteSelect :: SQLDatabase SQLStatement ![SQLValue] -> Task [SQLRow]
sqlExecuteSelect db query values = sqlExecute db [] (execSelect query values) 

sqlSelectShare :: String SQLStatement ![SQLValue] -> ROShared SQLDatabase [SQLRow]
sqlSelectShare name query values = createReadWriteSDS "SQLShares" name read write
where
	read db iworld
		# (mbOpen,iworld) = openMySQLDb db iworld
		= case mbOpen of
			Error e			= (Error e, iworld)
			Ok (cur,con,cxt)
				# (err,cur)			= execute query values cur
				| isJust err		= (Error (toString (fromJust err)),iworld)
				# (err,rows,cur)	= fetchAll cur
				| isJust err		= (Error (toString (fromJust err)),iworld)
				# iworld				= closeMySQLDb cur con cxt iworld
				= (Ok rows,iworld)

    write _ Void iworld = (Ok (const True),iworld)
		
openMySQLDb :: !SQLDatabase !*IWorld -> (MaybeErrorString (!*MySQLCursor, !*MySQLConnection, !*MySQLContext), !*IWorld)
openMySQLDb db iworld=:{IWorld|resources=Just (MySQLResource con)}
    = (Ok con, {IWorld|iworld & resources=Nothing})
openMySQLDb db iworld=:{IWorld|resources=Nothing}
            # iworld=:{IWorld|world} = {IWorld|iworld & resources = Nothing}
        	# (err,mbContext,world) 	= openContext world
        	| isJust err				= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
        	# (err,mbConn,context)		= openConnection db (fromJust mbContext)
        	| isJust err				= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
        	# (err,mbCursor,connection)	= openCursor (fromJust mbConn)
        	| isJust err				= (Error (toString (fromJust err)),{IWorld|iworld & world = world})
        	= (Ok (fromJust mbCursor,connection, context),{IWorld|iworld & world = world})
				
closeMySQLDb :: !*MySQLCursor !*MySQLConnection !*MySQLContext !*IWorld -> *IWorld
closeMySQLDb cursor connection context iworld=:{IWorld|resources=Nothing}
   = {IWorld|iworld & resources=Just (MySQLResource (cursor,connection,context))}
closeMySQLDb cursor connection context iworld=:{IWorld|world}
	# (err,connection)	= closeCursor cursor connection
	# (err,context) 	= closeConnection connection context
	# (err,world)		= closeContext context world
	= {IWorld|iworld & world = world}

