implementation module ProcessDB

import StdEnv, Maybe

import TaskContext, Store, Util, Text, Time
import SerializationGraphCopy //TODO: Make switchable from within iTasks module
 
derive JSONEncode	Process
derive JSONDecode	Process
gEq{|Process|} {processId = pid0} {processId = pid1} = pid0 == pid1
derive bimap Maybe, (,)

NEXT_ID_DB		:== "NextProcessID"
PROCESS_DB		:== "ProcessDB"
CONTEXT_DB id	:== "Process-" +++ toString id +++ "-context"

instance ProcessDB IWorld
where
	getNextProcessId :: !*IWorld -> (!ProcessId,!*IWorld)
	getNextProcessId iworld
		# (mbNewPid,iworld) = loadValue NEXT_ID_DB iworld
		= case mbNewPid of
			Just pid
				# iworld = storeValue NEXT_ID_DB (pid+1) iworld //increment the stored counter by 1
				= (pid,iworld)
			Nothing
				# iworld = storeValue NEXT_ID_DB 2 iworld //store the next value (2)
				= (1,iworld) //return the first value (1)
			
	getProcess :: !ProcessId !*IWorld -> (!Maybe Process,!*IWorld)
	getProcess processId iworld
		# (procs,iworld) 	= readProcessStore iworld
		= case [process \\ process <- procs | process.Process.processId == processId] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)
	
	getProcessContext :: !ProcessId !*IWorld -> (!Maybe TaskContext, !*IWorld)
	getProcessContext pid iworld
		= loadValue (CONTEXT_DB pid) iworld

	getProcessForUser :: !User !ProcessId !*IWorld -> (!Maybe Process,!*IWorld)
	getProcessForUser user processId iworld
		# (procs,iworld) 	= readProcessStore iworld
		= case [p\\ p <- procs |   p.Process.processId == processId
							   && user == p.Process.properties.ProcessProperties.managerProperties.worker
							      ] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)
				
	getProcesses :: ![TaskStatus] ![RunningTaskStatus] !*IWorld -> (![Process], !*IWorld)
	getProcesses statusses runningStatusses iworld 
		# (procs, iworld)	= readProcessStore iworld
		= ([p \\ p <- procs | isMember p.Process.properties.systemProperties.SystemProperties.status statusses && isMember p.Process.properties.ProcessProperties.managerProperties.ManagerProperties.status runningStatusses], iworld)
			
	getProcessesById :: ![ProcessId] !*IWorld -> (![Process], !*IWorld)
	getProcessesById ids iworld
		# (procs,iworld) 	= readProcessStore iworld
		= ([process \\ process <- procs | isMember process.Process.processId ids], iworld)
	
	getProcessesForUser	:: !User ![TaskStatus] ![RunningTaskStatus] !*IWorld -> (![Process], !*IWorld)
	getProcessesForUser user statusses runningStatusses iworld
		# (procs,iworld) 	= readProcessStore iworld
		= (filterProcs (\p -> isRelevant user p && isMember p.Process.properties.systemProperties.SystemProperties.status statusses && isMember p.Process.properties.ProcessProperties.managerProperties.ManagerProperties.status runningStatusses) procs, iworld)
	where
		isRelevant user {Process | properties}	
			//Either you are working on the task
			=  ( properties.ProcessProperties.managerProperties.worker == user)
		
	setProcessContext :: !ProcessId !TaskContext !*IWorld -> *IWorld
	setProcessContext processId context iworld
		//Store the context
		# iworld = storeValue (CONTEXT_DB processId) context iworld
		//Update the process table with the process information from this contexts
		# iworld = snd (processStore (update (contextToProcess processId context)) iworld)
		= iworld
	where
		update process [] = [process]
		update process [p:ps] = if (p.processId == process.processId) [process:ps] [p:update process ps]

	setProcessOwner	:: !User !ProcessId !*IWorld	-> (!Bool, !*IWorld)
	setProcessOwner worker taskId iworld
		= updateProcess taskId (\x -> {Process | x & properties = {ProcessProperties|x.Process.properties & managerProperties = {x.Process.properties.ProcessProperties.managerProperties & worker = worker}}}) iworld
		
	setProcessStatus :: !TaskStatus !ProcessId !*IWorld -> (!Bool,!*IWorld)
	setProcessStatus status taskId iworld
		= updateProcess taskId (\x -> {Process | x & properties = {x.Process.properties & systemProperties = {SystemProperties|x.Process.properties.systemProperties & status = status}}}) iworld

	updateProcess :: !ProcessId (Process -> Process) !*IWorld -> (!Bool, !*IWorld)
	updateProcess processId f iworld
		# (procs,iworld) 	= readProcessStore iworld
		# (nprocs,upd)		= unzip (map (update f) procs)
		# (nprocs,iworld)	= processStore (\_ -> nprocs) iworld
		= (or upd, iworld)
	where
		update f x		
			| x.Process.processId == processId	= (f x, True)
			| otherwise						= (x, False)
	
	updateProcessProperties :: !ProcessId (ProcessProperties -> ProcessProperties) !*IWorld -> (!Bool, !*IWorld)
	updateProcessProperties taskId f iworld = updateProcess taskId (\p -> {Process |p & properties = f p.Process.properties}) iworld

	deleteProcess :: !ProcessId !*IWorld	-> (!Bool, !*IWorld)
	deleteProcess processId iworld 
		//Delete values from store
		# iworld = deleteValues ("Process-" +++ toString processId) iworld
		//Delete from process table
		# (procs,iworld) 	= readProcessStore iworld
		# (nprocs,iworld)	= processStore (\_ -> [process \\ process <- procs | process.Process.processId <> processId]) iworld
		= (length procs <> length nprocs, iworld)
		
	lastChange :: !*IWorld -> (!Timestamp,!*IWorld)
	lastChange iworld
		# (mbTs,iworld) = getStoreTimestamp PROCESS_DB iworld
		= (fromMaybe (Timestamp 0) mbTs,iworld)


processStore ::  !([Process] -> [Process]) !*IWorld -> (![Process],!*IWorld) 
processStore fn iworld
	# (list,iworld)		= readProcessStore iworld
	# list 				= fn list
	# iworld			= storeValue "ProcessDB" list iworld 
	= (list,iworld)
	
readProcessStore :: !*IWorld -> (![Process],!*IWorld) 
readProcessStore iworld
	# (mbList,iworld)	= loadValue "ProcessDB" iworld
	= (fromMaybe [] mbList,iworld)

contextToProcess :: !ProcessId !TaskContext -> Process
contextToProcess processId (TaskContext properties _ scontext)
	= {processId = processId, properties = properties, subprocesses = tsubprocs scontext}
where
	tsubprocs (TTCRunning _ context)		= subprocs context
	tsubprocs _								= []

	subprocs (TCEmpty)						= []
	subprocs (TCBasic _)					= []
	subprocs (TCBind (Left context))		= subprocs context
	subprocs (TCBind (Right (_,context)))	= subprocs context
	subprocs (TCTry (Left context))			= subprocs context
	subprocs (TCTry (Right (_,context)))	= subprocs context
	subprocs (TCParallel _ _ subs)			= subprocsp subs
	
	subprocsp [] = []
	subprocsp [(_,STCDetached properties context):subs]
		= [{processId = processId
		   ,properties = properties
		   ,subprocesses = case context of Nothing = []; Just (_,c) = subprocs c}
		  :subprocsp subs]
	subprocsp [(_,STCBody _ Nothing):subs]			= subprocsp subs
	subprocsp [(_,STCBody _ (Just (_,c))):subs]		= subprocs c ++ subprocsp subs
	subprocsp [(_,STCHidden _ Nothing):subs]		= subprocsp subs
	subprocsp [(_,STCHidden _ (Just (_,c))):subs]	= subprocs c ++ subprocsp subs
					 
filterProcs :: (Process -> Bool) [Process] -> [Process]
filterProcs pred procs = flatten [if (pred p) [{p & subprocesses = filterProcs pred p.subprocesses}] (filterProcs pred p.subprocesses) \\  p <- procs]
