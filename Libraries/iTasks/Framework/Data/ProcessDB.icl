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
		# (procs,tst) 	= processStore id tst
		# newPid		= inc (maxPid procs)
		# (procs,tst)	= processStore (\_ -> procs ++ [{Process | entry & processId = newPid, properties = {TaskProperties| entry.properties & processId = newPid} }]) tst
		= (newPid, tst)
		
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
		#  uids			= [fst p.Process.properties.TaskProperties.user \\ p <- procs | relevantProc processId p]
		= case [p \\ p <- procs | p.Process.processId == processId && isMember userId uids] of
			[entry]	= (Just entry, tst)
			_		= (Nothing, tst)
	where
		relevantProc targetId {Process|processType = EmbeddedProcess pid _}	= pid == targetId 
		relevantProc targetId {Process|processId}							= processId == targetId
		relevantProc _ _													= False
			
	getProcesses :: ![ProcessStatus] !Bool !*TSt -> (![Process], !*TSt)
	getProcesses statusses ignoreEmbedded tst
		# (procs,tst) 	= processStore id tst
		| ignoreEmbedded
			= ([p \\ p <- procs | isMember p.Process.status statusses && not (isEmbedded p)], tst)
		| otherwise
			= ([p \\ p <- procs | isMember p.Process.status statusses], tst)
			
	getProcessesById :: ![ProcessId] !*TSt -> (![Process], !*TSt)
	getProcessesById ids tst
		# (procs,tst) 	= processStore id tst
		= ([process \\ process <- procs | isMember process.Process.processId ids], tst)

	getProcessesForUser	:: !UserId ![ProcessStatus] !Bool !*TSt -> (![Process], !*TSt)
	getProcessesForUser userId statusses ignoreEmbedded tst
		# (procs,tst) 	= processStore id tst
		#  procids		= filter ((<>) 0) (map (relevantProc userId) procs)
		= ([p \\ p <- procs | isMember p.Process.processId procids && isMember p.Process.status statusses], tst)

	where
		relevantProc userId {processType = EmbeddedProcess pid _, properties = {user}}
			| fst user == userId	= pid
			| otherwise				= 0
		relevantProc userId {processId, properties = {user}}
			| fst user == userId	= processId
			| otherwise				= 0
		
	getSubProcess :: !ProcessId !TaskId !*TSt -> (!Maybe Process,!*TSt)
	getSubProcess processId taskId tst
		# (procs,tst)	= processStore id tst
		= case [proc \\ proc =: {processType = (EmbeddedProcess pid tid)} <- procs | pid == processId && tid == taskId] of
			[entry] = (Just entry, tst)
			_		= (Nothing, tst)

	
	setProcessOwner	:: !(UserId, String) !(UserId,String) !ProcessId !*TSt	-> (!Bool, !*TSt)
	setProcessOwner user delegator processId tst
		= updateProcess processId (\x -> {Process| x & properties = {TaskProperties|x.properties & user = user, delegator = delegator}}) tst
	
	setProcessStatus :: !ProcessStatus !ProcessId !*TSt -> (!Bool,!*TSt)
	setProcessStatus status processId tst = updateProcess processId (\x -> {Process| x & status = status}) tst

	setProcessResult :: !DynamicId !ProcessId !*TSt -> (!Bool,!*TSt)
	setProcessResult result processId tst = updateProcess processId (\x -> {Process| x & result = Just result}) tst 
	
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


isEmbedded :: Process -> Bool
isEmbedded {processType = EmbeddedProcess _ _}	= True
isEmbedded _									= False

//Utility functions
mkStaticProcessEntry :: Workflow Timestamp (UserId,String) (UserId,String) ProcessStatus -> Process
mkStaticProcessEntry workflow timestamp user delegator status
	=	{ Process
		| processId		= 0
		, processType	= StaticProcess workflow.Workflow.name
		, parent		= 0
		, status		= status
		, properties	=	{ TaskProperties
							| processId		= 0
							, subject		= workflow.Workflow.label
							, user			= user
							, delegator		= delegator
							, priority		= NormalPriority
							, deadline		= Nothing
							, progress		= TPActive
							, issuedAt		= timestamp
							, firstEvent	= Nothing
							, latestEvent	= Nothing
							}
		, taskfun		= Nothing
		, result		= Nothing
		, changes		= []
		, changeNr		= 0
		}
		
mkDynamicProcessEntry :: String DynamicId Timestamp (UserId,String) (UserId,String) ProcessStatus ProcessId -> Process
mkDynamicProcessEntry label task timestamp user delegator status parent
	=	{ Process
		| processId	= 0
		, processType = DynamicProcess task
		, parent	= parent
		, status	= status
		, properties	=	{ TaskProperties
							| processId		= 0
							, subject		= label
							, user			= user
							, delegator		= delegator
							, priority		= NormalPriority
							, deadline		= Nothing
							, progress		= TPActive
							, issuedAt		= timestamp
							, firstEvent	= Nothing
							, latestEvent	= Nothing
							}
		, taskfun	= Nothing
		, result	= Nothing
		, changes	= []
		, changeNr	= 0
		}

mkEmbeddedProcessEntry	:: ProcessId TaskId TaskProperties ProcessStatus ProcessId	-> Process
mkEmbeddedProcessEntry ancestor taskid properties status parent
	=	{ Process
		| processId		= 0
		, processType	= EmbeddedProcess ancestor taskid
		, parent		= parent
		, status		= status
		, properties	= properties
		, taskfun		= Nothing
		, result		= Nothing
		, changes		= []
		, changeNr		= 0
		}		

processStore ::  !([Process] -> [Process]) !*TSt -> (![Process],!*TSt) 
processStore fn tst=:{store,world}
	# (mbList,store,world)	= loadValue "ProcessDB" store world
	# list 					= fn (case mbList of Nothing = []; Just list = list)
	# store					= storeValue "ProcessDB" list store 
	= (list, {tst & store = store, world = world})

maxPid :: [Process] -> ProcessId
maxPid db = foldr max 0 [processId \\ {Process|processId} <- db]

indexPid :: !ProcessId [Process] -> Int
indexPid pid db = indexPid` pid 0 db
where
	indexPid` pid i [] = -1
	indexPid` pid i [{Process|processId}:xs]
		| pid == processId	= i
							= indexPid` pid (i + 1) xs