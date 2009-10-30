implementation module ProcessDB

import StdEnv, StdGeneric, StdMaybe, GenBimap
import TSt, Store, DynamicDB, Util

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
	
instance ProcessDB TSt
where
	createProcess :: !Process !*TSt -> (!ProcessId,!*TSt)
	createProcess entry tst
		#(procs,tst)	= processStore id tst
		# pid = if (entry.Process.processId <> "") entry.Process.processId (getNewPid procs entry)
		# (procs,tst)	= processStore (\_ -> procs ++ [{Process | entry & processId = pid, properties = {TaskProperties| entry.properties & systemProps = {TaskSystemProperties|entry.properties.systemProps & processId = pid}} }]) tst
		= (pid, tst)
		
	deleteProcess :: !ProcessId !*TSt	-> (!Bool, !*TSt)
	deleteProcess processId tst 
		# (procs,tst) 	= processStore id tst
		# (nprocs,tst)	= processStore (\_ -> [process \\ process <- procs | process.Process.processId <> processId]) tst
		= (length procs <> length nprocs, tst)
		
	getProcess :: !ProcessId !*TSt -> (!Maybe Process,!*TSt)
	getProcess processId tst
		# (procs,tst) 	= processStore id tst
		= case [process \\ process <- procs | process.Process.processId == processId] of
			[entry]	= (Just entry, tst)
			_		= (Nothing, tst)

	getProcessForUser :: !UserId !ProcessId !*TSt -> (!Maybe Process,!*TSt)
	getProcessForUser userId processId tst
		# (procs,tst) 	= processStore id tst
		#  uids			= [fst p.Process.properties.managerProps.worker \\ p <- procs | relevantProc processId p]
		= case [p \\ p <- procs | p.Process.processId == processId && isMember userId uids] of
			[entry]	= (Just entry, tst)
			_		= (Nothing, tst)
	where
		//relevantProc targetId {Process|processType = EmbeddedProcess pid _}	= pid == targetId 
		relevantProc targetId {Process|processId}							= processId == targetId
		relevantProc _ _													= False
	/*		
	getProcesses :: ![ProcessStatus] !Bool !*TSt -> (![Process], !*TSt)
	getProcesses statusses ignoreEmbedded tst
		# (procs,tst) 	= processStore id tst
		| ignoreEmbedded
			= ([p \\ p <- procs | isMember p.Process.status statusses && not (isEmbedded p)], tst)
		| otherwise
			= ([p \\ p <- procs | isMember p.Process.status statusses], tst)
	*/
	
	getProcesses :: ![ProcessStatus] !*TSt -> (![Process], !*TSt)
	getProcesses statusses tst 
		# (procs, tst)	= processStore id tst
		= ([p \\ p <- procs | isMember p.Process.status statusses], tst)
			
	getProcessesById :: ![ProcessId] !*TSt -> (![Process], !*TSt)
	getProcessesById ids tst
		# (procs,tst) 	= processStore id tst
		= ([process \\ process <- procs | isMember process.Process.processId ids], tst)

	getProcessesForUser	:: !UserId ![ProcessStatus] !Bool !*TSt -> (![Process], !*TSt)
	getProcessesForUser userId statusses ignoreEmbedded tst
		# (procs,tst) 	= processStore id tst
		# rprocs	 	= map (relevantProc userId) procs
		# procids		= [p \\ p <- rprocs | p <> ""]
		//filter ((<>) "") (map (relevantProc userId) procs)
		= ([p \\ p <- procs | isMember p.Process.processId procids && isMember p.Process.status statusses], tst)
	where
		//relevantProc userId {processId, processType = EmbeddedProcess pid _, properties = {managerProps ={worker}}}
		//	| fst worker == userId	= pid
		//	| otherwise				= ""
		relevantProc userId {processId, properties = {managerProps = {worker}}}
			| fst worker == userId	= processId
			| otherwise				= ""
	
	/*	
	getSubProcess :: !ProcessId !TaskId !*TSt -> (!Maybe Process,!*TSt)
	getSubProcess processId taskId tst
		# (procs,tst)	= processStore id tst
		= case [proc \\ proc =: {processType = (EmbeddedProcess pid tid)} <- procs | pid == processId && tid == taskId] of
			[entry] = (Just entry, tst)
			_		= (Nothing, tst)
	*/

	
	setProcessOwner	:: !(UserId, String) !(UserId,String) !ProcessId !*TSt	-> (!Bool, !*TSt)
	setProcessOwner worker manager processId tst
		= updateProcess processId (\x -> {Process| x & properties = {TaskProperties|x.properties & systemProps = {x.properties.systemProps & manager = manager}, managerProps = {x.properties.managerProps & worker = worker}}}) tst
	
	setProcessStatus :: !ProcessStatus !ProcessId !*TSt -> (!Bool,!*TSt)
	setProcessStatus status processId tst = updateProcess processId (\x -> {Process| x & status = status}) tst

	//setProcessResult :: !DynamicId !ProcessId !*TSt -> (!Bool,!*TSt)
	//setProcessResult result processId tst = updateProcess processId (\x -> {Process| x & result = Just result}) tst 
	
	updateProcess :: ProcessId (Process -> Process) !*TSt -> (!Bool, !*TSt)
	updateProcess processId f tst
		# (procs,tst) 	= processStore id tst
		# (nprocs,upd)	= unzip (map (update f) procs)
		# (nprocs,tst)	= processStore (\_ -> nprocs) tst
		= (or upd, tst)
	where
		update f x		
			| x.Process.processId == processId	= (f x, True)
			| otherwise							= (x, False)

	updateProcessProperties :: !ProcessId (TaskProperties -> TaskProperties) !*TSt -> (!Bool, !*TSt)
	updateProcessProperties processId f tst = updateProcess processId (\p -> {Process |p & properties = f p.properties}) tst

	removeFinishedProcesses :: !*TSt -> (!Bool, !*TSt)
	removeFinishedProcesses tst
	# (proc,tst) = getProcesses [Finished] tst
	= removeFinishedProcesses` proc tst
	where
		removeFinishedProcesses` :: ![Process] !*TSt -> (!Bool, !*TSt)
		removeFinishedProcesses` [] tst = (True, tst)
		removeFinishedProcesses` [p:ps] tst
		# (ok,tst)  = deleteProcess p.Process.processId tst
		# tst		= deleteTaskStates (taskNrFromString p.Process.processId) tst
		| ok 		= removeFinishedProcesses` ps tst
		| otherwise = (False,tst) 

//isEmbedded :: Process -> Bool
//isEmbedded {processType = EmbeddedProcess _ _}	= True
//isEmbedded _									= False

//Utility functions
mkProcessEntry :: String Timestamp (UserId, String) (UserId, String) ProcessStatus ProcessId -> Process
mkProcessEntry label timestamp user delegator status parent
	=	{ Process
		| processId		= ""
		, status		= status
		, parent		= parent
		, properties	=	{ systemProps =
							  {TaskSystemProperties
							  | processId	= ""
							  , subject		= label
							  , manager		= delegator
							  , issuedAt	= timestamp
							  , firstEvent	= Nothing
							  , latestEvent	= Nothing
							  }
							, managerProps =
							  { TaskManagerProperties
							  | worker		= user
							  , priority	= NormalPriority
							  , deadline	= Nothing
							  }
							, workerProps =
							  { TaskWorkerProperties
							  | progress	= TPActive
							  }
							}
		, changes		= []
		, changeNr		= 0
		}
/*
mkStaticProcessEntry :: Workflow Timestamp (UserId,String) (UserId,String) ProcessStatus -> Process
mkStaticProcessEntry workflow timestamp user delegator status
	=	{ Process
		| processId		= ""
		, processType	= StaticProcess workflow.Workflow.name
		, parent		= ""
		, status		= status
		, properties	=	{ systemProps =
							  {TaskSystemProperties
							  | processId	= ""
							  , subject		= workflow.Workflow.label
							  , manager		= delegator
							  , issuedAt	= timestamp
							  , firstEvent	= Nothing
							  , latestEvent	= Nothing
							  }
							, managerProps =
							  { TaskManagerProperties
							  | worker		= user
							  , priority	= NormalPriority
							  , deadline	= Nothing
							  }
							, workerProps =
							  { TaskWorkerProperties
							  | progress	= TPActive
							  }
							}
		, taskfun		= Nothing
		, result		= Nothing
		, changes		= []
		, changeNr		= 0
		}
		
mkDynamicProcessEntry :: String DynamicId Timestamp (UserId,String) (UserId,String) ProcessStatus ProcessId -> Process
mkDynamicProcessEntry label task timestamp user delegator status parent
	=	{ Process
		| processId	= ""
		, processType = DynamicProcess task
		, parent	= parent
		, status	= status
		, properties=	{ systemProps =
							  {TaskSystemProperties
							  | processId	= ""
							  , subject		= label
							  , manager		= delegator
							  , issuedAt	= timestamp
							  , firstEvent	= Nothing
							  , latestEvent	= Nothing
							  }
							, managerProps =
							  { TaskManagerProperties
							  | worker		= user
							  , priority	= NormalPriority
							  , deadline	= Nothing
							  }
							, workerProps =
							  { TaskWorkerProperties
							  | progress	= TPActive
							  }
							}
		, taskfun	= Nothing
		, result	= Nothing
		, changes	= []
		, changeNr	= 0
		}

mkEmbeddedProcessEntry	:: ProcessId TaskId TaskProperties ProcessStatus ProcessId	-> Process
mkEmbeddedProcessEntry ancestor taskid properties status parent
	=	{ Process
		| processId		= ""
		, processType	= EmbeddedProcess ancestor taskid
		, parent		= parent
		, status		= status
		, properties	= properties
		, taskfun		= Nothing
		, result		= Nothing
		, changes		= []
		, changeNr		= 0
		}		
*/

processStore ::  !([Process] -> [Process]) !*TSt -> (![Process],!*TSt) 
processStore fn tst=:{TSt|dataStore,world}
	# (mbList,dstore,world)	= loadValue "ProcessDB" dataStore world
	# list 					= fn (case mbList of Nothing = []; Just list = list)
	# dstore				= storeValue "ProcessDB" list dstore 
	= (list, {TSt|tst & dataStore = dstore, world = world})

maxPid :: [Process] -> Int
maxPid db = foldr max 0 (map (last o taskNrFromString) [processId \\ {Process|processId} <- db])

getNewPid :: ![Process] !Process -> ProcessId
getNewPid db entry = (toString(inc(maxPid db)))

/*
getNewPid :: ![Process] !Process -> ProcessId
getNewPid db entry = 
| isEmbedded entry
	= fetchTaskNr entry
| otherwise
	= (toString(inc(maxPid db)))
where
	fetchTaskNr entry =
		case entry.processType of
		(EmbeddedProcess an tid) = tid
		_ = abort "Not an Embedded process"*/
							