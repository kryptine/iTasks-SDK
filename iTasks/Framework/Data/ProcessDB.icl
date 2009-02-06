implementation module ProcessDB

import StdEnv, StdGeneric, StdMaybe, GenBimap
import HSt, TSt, iDataForms, iDataFormlib

//We now use a fixed ProcessDB type which stores
//the complete list of processes in memory
//The best solution would be to use an overloaded
//ProcesDB type which can lazily retrieve data

:: *ProcessDB = ProcessDB [Process]

derive gForm	Process, StaticProcessEntry, DynamicProcessEntry, ProcessStatus
derive gUpd		Process, StaticProcessEntry, DynamicProcessEntry, ProcessStatus
derive gPrint	Process, StaticProcessEntry, DynamicProcessEntry, ProcessStatus
derive gParse	Process, StaticProcessEntry, DynamicProcessEntry, ProcessStatus

instance == ProcessStatus
where
	(==) Active		Active		= True
	(==) Suspended	Suspended	= True
	(==) Finished	Finished	= True
	(==) Deleted	Deleted		= True
	(==) _			_			= False

instance toString ProcessStatus
where
	toString Active		= "Active"
	toString Suspended	= "Suspended"
	toString Finished	= "Finished"
	toString Deleted	= "Deleted"

class ProcessDBEnv env
where
	openProcessDB		:: !*env -> (!*ProcessDB, !*env)
	closeProcessDB		:: !*ProcessDB !*env -> *env

instance ProcessDBEnv HSt
where
	openProcessDB :: !*HSt -> (!*ProcessDB, !*HSt)
	openProcessDB hst
		# (db,hst) = processStore id hst
		= (ProcessDB db, hst)

	closeProcessDB :: !*ProcessDB !*HSt -> *HSt	
	closeProcessDB (ProcessDB db) hst
		#(_,hst)	= processStore (\_ -> db) hst
		= hst
		
processStore ::  !([Process] -> [Process]) !*HSt -> (![Process],!*HSt) 
processStore fn hst		
	# (form,hst) = mkStoreForm (Init, pFormId "ProcessTable" []) fn hst
	= (form.Form.value, hst)

createProcess :: !Process			!*ProcessDB	-> (!Int, 				!*ProcessDB)
createProcess entry (ProcessDB db) = (newid, ProcessDB newlist)
where
	newid	= (maxPid db) + 1
	newlist	= db ++ [{Process | entry & id = newid}]

deleteProcess :: !Int						!*ProcessDB	-> (!Bool,				!*ProcessDB)
deleteProcess processId (ProcessDB db) = (deleted, ProcessDB newlist)
where
	deleted	= length db == (length newlist) - 1
	newlist	= [process \\ process <- db | process.Process.id <> processId]	
	
getProcess :: !Int						!*ProcessDB -> (!Maybe Process,!*ProcessDB)
getProcess processId (ProcessDB db)
	= case [process \\ process <- db | process.Process.id == processId] of
		[entry]	= (Just entry, (ProcessDB db))
		_		= (Nothing, (ProcessDB db))

getProcessForUser	:: !Int !Int !*ProcessDB	-> (!Maybe Process,!*ProcessDB)
getProcessForUser userId processId (ProcessDB db)
	= case [process \\ process <- db | process.Process.id == processId && (process.Process.owner == userId || isMember userId process.Process.users)] of
		[entry]	= (Just entry, (ProcessDB db))
		_		= (Nothing, (ProcessDB db))
		
getProcesses :: ![ProcessStatus] !*ProcessDB -> (![Process],	!*ProcessDB)
getProcesses statusses (ProcessDB db) = (entries,(ProcessDB db))
where
	entries = [process \\ process <- db | isMember process.Process.status statusses]		

getProcessesById :: ![Int] !*ProcessDB -> (![Process],!*ProcessDB)
getProcessesById ids (ProcessDB db) = (entries,(ProcessDB db))
where
	entries = [process \\ process <- db | isMember process.Process.id ids]

getProcessesForUser	:: !Int ![ProcessStatus] !*ProcessDB -> (![Process],	!*ProcessDB)
getProcessesForUser userId statusses (ProcessDB db) = (entries,(ProcessDB db))
where
	entries = [process \\ process <- db | (process.Process.owner == userId || isMember userId process.Process.users) && isMember process.Process.status statusses]

setProcessOwner	:: !Int !Int !*ProcessDB	-> (!Bool,				!*ProcessDB)
setProcessOwner userId processId (ProcessDB db) = (updated, ProcessDB newlist)
where
	updated				= or updates
	(updates,newlist)	= unzip (map update db)
	update entry		
		| entry.Process.id == processId	= (True, {entry & owner = userId})
												= (False, entry)

setProcessStatus :: !ProcessStatus !Int		!*ProcessDB	-> (!Bool,				!*ProcessDB)
setProcessStatus status processId (ProcessDB db) = (updated, ProcessDB newlist)
where
	updated				= or updates
	(updates,newlist)	= unzip (map update db)
	update entry
		| entry.Process.id == processId	= (True, {Process|entry & status = status})
												= (False, entry)

setProcessResult	:: !String !Int				!*ProcessDB -> (!Bool,				!*ProcessDB)
setProcessResult result processId (ProcessDB db) = (updated, ProcessDB newlist)
where
	(updated,newlist) 	= setProcessResult` result processId db
		
	setProcessResult` res pid [] = (False, [])
	setProcessResult` res pid [x:xs]
		= case x.Process.process of
			(LEFT staticProc)
				# (found, xs)	= setProcessResult` res pid xs
				= (found, [x:xs])
			(RIGHT dynamicProc)
				| x.Process.id == pid
					= (True, [{x & process = (RIGHT {dynamicProc & result = res})}:xs])
				| otherwise
					# (found, xs)	= setProcessResult` res pid xs
					= (found, [x:xs])

updateProcess :: !ProcessStatus !(Maybe String) ![UserId] !ProcessId !*ProcessDB -> (!Bool,!*ProcessDB) 
updateProcess status mbResult users processId (ProcessDB db) = (updated, ProcessDB newlist)
where
	(updated, newlist)	= updateProcess` status mbResult users processId db
	
	updateProcess` stat mbres usr pid []	= (False, [])
	updateProcess` stat mbres usr pid [x:xs]
		| x.Process.id == pid
			= case x.Process.process of
				(LEFT staticProc)
					= (True, [{Process | x & status = stat, users = usr, process = LEFT staticProc}:xs]) 
				(RIGHT dynamicProc)
					= case mbres of
						Just res	= (True, [{Process | x & status = stat, users = usr, process = RIGHT {dynamicProc & result = res}}:xs])
						Nothing		= (True, [{Process | x & status = stat, users = usr, process = RIGHT dynamicProc}:xs])
		| otherwise
			# (found, xs)	= updateProcess` stat mbres usr pid xs
			= (found, [x:xs])


//Utility functions
createStaticProcessEntry :: Workflow Int ProcessStatus -> Process
createStaticProcessEntry workflow owner status
	=	{ Process
		| id		= 0
		, owner		= owner
		, users		= [owner]
		, status	= status
		, process	= LEFT {workflow = workflow.name}
		}
createDynamicProcessEntry :: String String Int ProcessStatus Int-> Process
createDynamicProcessEntry label task owner status parent
	=	{ Process
		| id		= 0
		, owner		= owner
		, users		= [owner]
		, status	= status
		, process	= RIGHT {label = label, result = "", task = task, parent = parent}
		}

maxPid :: [Process] -> Int
maxPid db = foldr max 0 [id \\ {Process|id} <- db]

indexPid :: !Int [Process] -> Int
indexPid pid db = indexPid` pid 0 db
where
	indexPid` pid i [] = -1
	indexPid` pid i [{Process|id}:xs]
		| pid == id	= i
					= indexPid` pid (i + 1) xs