implementation module ProcessDB

import StdEnv, StdGeneric, StdMaybe, GenBimap
import HSt, TSt, iDataForms, iDataFormlib
import DynamicDB

derive gForm	Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, TaskProgress, Time
derive gUpd		Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, TaskProgress, Time
derive gPrint	Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, TaskProgress, Time
derive gParse	Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, TaskProgress, Time

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
	createProcess :: !Process !*HSt -> (!ProcessId,!*HSt)
	createProcess entry hst 
		# (procs,hst) 	= processStore id hst
		# newPid		= inc (maxPid procs)
		# (procs,hst)	= processStore (\_ -> procs ++ [{Process | entry & processId = newPid, properties = {TaskProperties| entry.properties & processId = newPid} }]) hst
		= (newPid, hst)
		
	deleteProcess :: !ProcessId !*HSt	-> (!Bool, !*HSt)
	deleteProcess processId hst 
		# (procs,hst) 	= processStore id hst
		# (nprocs,hst)	= processStore (\_ -> [process \\ process <- procs | process.Process.processId <> processId]) hst
		= (length procs <> length nprocs, hst)
		
	getProcess :: !ProcessId !*HSt -> (!Maybe Process,!*HSt)
	getProcess processId hst
		# (procs,hst) 	= processStore id hst
		= case [process \\ process <- procs | process.Process.processId == processId] of
			[entry]	= (Just entry, hst)
			_		= (Nothing, hst)

	getProcessForUser :: !UserId !ProcessId !*HSt -> (!Maybe Process,!*HSt)
	getProcessForUser userId processId hst
		# (procs,hst) 	= processStore id hst
		#  uids			= [fst p.Process.properties.TaskProperties.user \\ p <- procs | relevantProc processId p]
		= case [p \\ p <- procs | p.Process.processId == processId && isMember userId uids] of
			[entry]	= (Just entry, hst)
			_		= (Nothing, hst)
	where
		relevantProc targetId {Process|processType = EmbeddedProcess pid _}	= pid == targetId 
		relevantProc targetId {Process|processId}							= processId == targetId
		relevantProc _ _													= False
			
	getProcesses :: ![ProcessStatus] !*HSt -> (![Process], !*HSt)
	getProcesses statusses hst
		# (procs,hst) 	= processStore id hst
		= ([process \\ process <- procs | isMember process.Process.status statusses], hst)
	
	getProcessesById :: ![ProcessId] !*HSt -> (![Process], !*HSt)
	getProcessesById ids hst
		# (procs,hst) 	= processStore id hst
		= ([process \\ process <- procs | isMember process.Process.processId ids], hst)

	getProcessesForUser	:: !UserId ![ProcessStatus] !Bool !*HSt -> (![Process], !*HSt)
	getProcessesForUser userId statusses ignoreEmbedded hst
		# (procs,hst) 	= processStore id hst
		#  procids		= filter ((<>) 0) (map (relevantProc userId) procs)
		= ([p \\ p <- procs | isMember p.Process.processId procids && isMember p.Process.status statusses], hst)

	where
		relevantProc userId {processType = EmbeddedProcess pid _, properties = {user}}
			| fst user == userId	= pid
			| otherwise				= 0
		relevantProc userId {processId, properties = {user}}
			| fst user == userId	= processId
			| otherwise				= 0
		
	getSubProcess :: !ProcessId !TaskId !*HSt -> (!Maybe Process,!*HSt)
	getSubProcess processId taskId hst
		# (procs,hst)	= processStore id hst
		= case [proc \\ proc =: {processType = (EmbeddedProcess pid tid)} <- procs | pid == processId && tid == taskId] of
			[entry] = (Just entry, hst)
			_		= (Nothing, hst)

	
	setProcessOwner	:: !(UserId, String) !(UserId,String) !ProcessId !*HSt	-> (!Bool, !*HSt)
	setProcessOwner user delegator processId hst
		= updateProcess processId (\x -> {Process| x & properties = {TaskProperties|x.properties & user = user, delegator = delegator}}) hst
	
	setProcessStatus :: !ProcessStatus !ProcessId !*HSt -> (!Bool,!*HSt)
	setProcessStatus status processId hst = updateProcess processId (\x -> {Process| x & status = status}) hst

	setProcessResult :: !DynamicId !ProcessId !*HSt -> (!Bool,!*HSt)
	setProcessResult result processId hst = updateProcess processId (\x -> {Process| x & result = Just result}) hst 
	
	updateProcess :: ProcessId (Process -> Process) !*HSt -> (!Bool, !*HSt)
	updateProcess processId f hst
		# (procs,hst) 	= processStore id hst
		# (nprocs,upd)	= unzip (map (update f) procs)
		# (nprocs,hst)	= processStore (\_ -> nprocs) hst
		= (or upd, hst)
	where
		update f x		
			| x.Process.processId == processId	= (f x, True)
			| otherwise							= (x, False)

	updateProcessProperties :: !ProcessId (TaskProperties -> TaskProperties) !*HSt -> (!Bool, !*HSt)
	updateProcessProperties processId f hst = updateProcess processId (\p -> {Process |p & properties = f p.properties}) hst

instance ProcessDB TSt
where
	createProcess entry tst									= accHStTSt (createProcess entry) tst
	deleteProcess processId tst								= accHStTSt (deleteProcess processId) tst
	getProcess processId tst								= accHStTSt (getProcess processId) tst
	getProcessForUser userId processId tst					= accHStTSt (getProcessForUser userId processId) tst
	getProcesses statusses tst								= accHStTSt (getProcesses statusses) tst
	getProcessesById ids tst								= accHStTSt (getProcessesById ids) tst
	getProcessesForUser userId statusses ignoreEmbedded tst	= accHStTSt (getProcessesForUser userId statusses ignoreEmbedded) tst
	getSubProcess processId taskId tst						= accHStTSt (getSubProcess processId taskId) tst
	setProcessOwner user delegator processId tst			= accHStTSt (setProcessOwner user delegator processId) tst
	setProcessStatus status processId tst					= accHStTSt (setProcessStatus status processId) tst
	setProcessResult result processId tst					= accHStTSt (setProcessResult result processId) tst
	updateProcess processId f tst							= accHStTSt (updateProcess processId f) tst
	updateProcessProperties processId f tst					= accHStTSt (updateProcessProperties processId f) tst


//Utility functions
mkStaticProcessEntry :: Workflow Time (UserId,String) (UserId,String) ProcessStatus -> Process
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
		}
		
mkDynamicProcessEntry :: String DynamicId Time (UserId,String) (UserId,String) ProcessStatus ProcessId -> Process
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
		}		

processStore ::  !([Process] -> [Process]) !*HSt -> (![Process],!*HSt) 
processStore fn hst		
	# (form,hst) = mkStoreForm (Init, pFormId "ProcessDB" []) fn hst
	= (form.Form.value, hst)

maxPid :: [Process] -> ProcessId
maxPid db = foldr max 0 [processId \\ {Process|processId} <- db]

indexPid :: !ProcessId [Process] -> Int
indexPid pid db = indexPid` pid 0 db
where
	indexPid` pid i [] = -1
	indexPid` pid i [{Process|processId}:xs]
		| pid == processId	= i
							= indexPid` pid (i + 1) xs