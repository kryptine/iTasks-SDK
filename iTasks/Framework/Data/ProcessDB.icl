implementation module ProcessDB

import StdEnv, StdGeneric, StdMaybe, GenBimap
import HSt, TSt, iDataForms, iDataFormlib

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

instance ProcessDB HSt
where
	createProcess :: !Process !*HSt -> (!Int,!*HSt)
	createProcess entry hst 
		# (procs,hst) 	= processStore id hst
		# newPid		= inc (maxPid procs)
		# (procs,hst)	= processStore (\_ -> procs ++ [{Process | entry & id = newPid}]) hst
		= (newPid, hst)
		
	deleteProcess :: !Int !*HSt	-> (!Bool, !*HSt)
	deleteProcess processId hst 
		# (procs,hst) 	= processStore id hst
		# (nprocs,hst)	= processStore (\_ -> [process \\ process <- procs | process.Process.id <> processId]) hst
		= (length procs <> length nprocs, hst)
		
	getProcess :: !Int !*HSt -> (!Maybe Process,!*HSt)
	getProcess processId hst
		# (procs,hst) 	= processStore id hst
		= case [process \\ process <- procs | process.Process.id == processId] of
			[entry]	= (Just entry, hst)
			_		= (Nothing, hst)

	getProcessForUser :: !Int !Int !*HSt -> (!Maybe Process,!*HSt)
	getProcessForUser userId processId hst
		# (procs,hst) 	= processStore id hst
		= case [process \\ process <- procs | process.Process.id == processId && (process.Process.owner == userId || isMember userId process.Process.users)] of
			[entry]	= (Just entry, hst)
			_		= (Nothing, hst)
			
	getProcesses :: ![ProcessStatus] !*HSt -> (![Process], !*HSt)
	getProcesses statusses hst
		# (procs,hst) 	= processStore id hst
		= ([process \\ process <- procs | isMember process.Process.status statusses], hst)
	
	getProcessesById :: ![Int] !*HSt -> (![Process], !*HSt)
	getProcessesById ids hst
		# (procs,hst) 	= processStore id hst
		= ([process \\ process <- procs | isMember process.Process.id ids], hst)

	getProcessesForUser	:: !Int ![ProcessStatus] !*HSt -> (![Process], !*HSt)
	getProcessesForUser userId statusses hst
		# (procs,hst) 	= processStore id hst
		= ([process \\ process <- procs | (process.Process.owner == userId || isMember userId process.Process.users) && isMember process.Process.status statusses], hst)


	
	setProcessOwner	:: !Int !Int !Int !*HSt	-> (!Bool, !*HSt)
	setProcessOwner userId delegatorId processId hst = setProcessProperty (\x -> {Process| x & owner = userId, delegator = delegatorId}) processId hst
	
	setProcessStatus :: !ProcessStatus !Int	!*HSt -> (!Bool,!*HSt)
	setProcessStatus status processId hst = setProcessProperty (\x -> {Process| x & status = status}) processId hst

	setProcessResult :: !String !Int !*HSt -> (!Bool,!*HSt)
	setProcessResult result processId hst = setProcessProperty (update result) processId hst 
	where
		update result x = case x.Process.process of
			(LEFT staticProc)
				= x 
			(RIGHT dynamicProc)
				= {x & process = (RIGHT {dynamicProc & result = result})}

	updateProcess :: !ProcessStatus !(Maybe String) ![UserId] !ProcessId !*HSt -> (!Bool,!*HSt) 
	updateProcess status mbResult users processId hst = setProcessProperty (update status mbResult users) processId hst 
	where
		update status mbResult users x = case x.Process.process of
			(LEFT staticProc)
				= {Process | x & status = status, users = users, process = LEFT staticProc} 
			(RIGHT dynamicProc)
				= case mbResult of
					Just result	= {Process | x & status = status, users = users, process = RIGHT {dynamicProc & result = result}}
					Nothing		= {Process | x & status = status, users = users, process = RIGHT dynamicProc}
	
//Generic process property update function
setProcessProperty :: (Process -> Process) Int *HSt -> (!Bool, *HSt)
setProcessProperty f processId hst
	# (procs,hst) 	= processStore id hst
	# (nprocs,upd)	= unzip (map (update f) procs)
	# (nprocs,hst)	= processStore (\_ -> nprocs) hst
	= (or upd, hst)
where
	update f x		
		| x.Process.id == processId		= (f x, True)
		| otherwise						= (x, False)

//Utility functions
mkStaticProcessEntry :: Workflow UserId UserId ProcessStatus -> Process
mkStaticProcessEntry workflow owner delegator status
	=	{ Process
		| id		= 0
		, label		= workflow.name
		, owner		= owner
		, delegator	= delegator
		, users		= []
		, status	= status
		, process	= LEFT {workflow = workflow.name}
		}
mkDynamicProcessEntry :: String String UserId UserId ProcessStatus Int-> Process
mkDynamicProcessEntry label task owner delegator status parent
	=	{ Process
		| id		= 0
		, label		= label
		, owner		= owner
		, delegator	= delegator
		, users		= []
		, status	= status
		, process	= RIGHT {result = "", task = task, parent = parent}
		}
		
processStore ::  !([Process] -> [Process]) !*HSt -> (![Process],!*HSt) 
processStore fn hst		
	# (form,hst) = mkStoreForm (Init, pFormId "ProcessDB" []) fn hst
	= (form.Form.value, hst)

maxPid :: [Process] -> Int
maxPid db = foldr max 0 [id \\ {Process|id} <- db]

indexPid :: !Int [Process] -> Int
indexPid pid db = indexPid` pid 0 db
where
	indexPid` pid i [] = -1
	indexPid` pid i [{Process|id}:xs]
		| pid == id	= i
					= indexPid` pid (i + 1) xs