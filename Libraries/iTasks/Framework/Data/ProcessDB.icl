implementation module ProcessDB

import StdEnv, StdGeneric, StdMaybe, GenBimap
import HSt, TSt, iDataForms, iDataFormlib
import DynamicDB

derive gForm	Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, Time
derive gUpd		Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, Time
derive gPrint	Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, Time
derive gParse	Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, Time

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
		# (procs,hst)	= processStore (\_ -> procs ++ [{Process | entry & processId = newPid}]) hst
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
		= case [process \\ process <- procs | process.Process.processId == processId && (process.Process.properties.TaskProperties.userId == userId || isMember userId process.Process.users)] of
			[entry]	= (Just entry, hst)
			_		= (Nothing, hst)
			
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
		= ([process \\ process <- procs |  not (isEmbedded process)	
										&& (process.Process.properties.TaskProperties.userId == userId || isMember userId process.Process.users)
										&& isMember process.Process.status statusses], hst)

	where
		isEmbedded {processType = EmbeddedProcess _ _}	= True
		isEmbedded _									= False

	getEmbeddedProcess	:: !ProcessId !TaskId !*HSt -> (!Maybe Process,!*HSt)
	getEmbeddedProcess processId taskId hst
		# (procs,hst)	= processStore id hst
		= case [proc \\ proc =: {processType = (EmbeddedProcess pid tid)} <- procs | pid == processId && tid == taskId] of
			[entry] = (Just entry, hst)
			_		= (Nothing, hst)

	
	setProcessOwner	:: !UserId !UserId !ProcessId !*HSt	-> (!Bool, !*HSt)
	setProcessOwner userId delegatorId processId hst
		= setProcessProperty (\x -> {Process| x & properties = {TaskProperties|x.properties & userId = userId, delegatorId = delegatorId}}) processId hst
	
	setProcessStatus :: !ProcessStatus !ProcessId !*HSt -> (!Bool,!*HSt)
	setProcessStatus status processId hst = setProcessProperty (\x -> {Process| x & status = status}) processId hst

	setProcessResult :: !DynamicId !ProcessId !*HSt -> (!Bool,!*HSt)
	setProcessResult result processId hst = setProcessProperty (\x -> {Process| x & result = Just result}) processId hst 
	
	updateProcess :: !ProcessStatus !(Maybe DynamicId) ![UserId] !ProcessId !*HSt -> (!Bool,!*HSt) 
	updateProcess status mbResult users processId hst = setProcessProperty (\x -> {Process| x & status = status, result = mbResult, users = users}) processId hst 

	
//Generic process property update function
setProcessProperty :: (Process -> Process) ProcessId *HSt -> (!Bool, *HSt)
setProcessProperty f processId hst
	# (procs,hst) 	= processStore id hst
	# (nprocs,upd)	= unzip (map (update f) procs)
	# (nprocs,hst)	= processStore (\_ -> nprocs) hst
	= (or upd, hst)
where
	update f x		
		| x.Process.processId == processId	= (f x, True)
		| otherwise							= (x, False)

//Utility functions
mkStaticProcessEntry :: Workflow UserId UserId ProcessStatus -> Process
mkStaticProcessEntry workflow owner delegator status
	=	{ Process
		| processId		= 0
		, processType	= StaticProcess workflow.Workflow.name
		, parent		= 0
		, status		= status
		, properties	=	{ TaskProperties
							| subject		= workflow.Workflow.label
							, userId		= owner
							, delegatorId	= delegator
							, priority		= NormalPriority
							, issuedAt		= Time 0 //TODO
							, firstEvent	= Nothing
							, latestEvent	= Nothing
							}
		, result		= Nothing
		, users			= []
		}
		
mkDynamicProcessEntry :: String DynamicId UserId UserId ProcessStatus ProcessId -> Process
mkDynamicProcessEntry label task owner delegator status parent
	=	{ Process
		| processId	= 0
		, processType = DynamicProcess task
		, parent	= parent
		, status	= status
		, properties	=	{ TaskProperties
							| subject		= label
							, userId		= owner
							, delegatorId	= delegator
							, priority		= NormalPriority
							, issuedAt		= Time 0 //TODO
							, firstEvent	= Nothing
							, latestEvent	= Nothing
							}
		, result	= Nothing
		, users		= []
		}

mkEmbeddedProcessEntry	:: ProcessId TaskId TaskProperties ProcessStatus ProcessId	-> Process
mkEmbeddedProcessEntry ancestor taskid properties status parent
	=	{ Process
		| processId		= 0
		, processType	= EmbeddedProcess ancestor taskid
		, parent		= parent
		, status		= status
		, properties	= properties
		, result		= Nothing
		, users			= []
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