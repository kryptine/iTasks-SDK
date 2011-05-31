implementation module ProcessDB

import StdEnv, Maybe

import TaskContext, Store, Util, Text
import SerializationGraphCopy //TODO: Make switchable from within iTasks module
 
derive JSONEncode	Process
derive JSONDecode	Process
derive gEq			Process
derive bimap Maybe, (,)

NEXT_ID_DB		:== "NextProcessID"
PROCESS_DB		:== "ProcessDB"
THREAD_DB id	:== "Process-" +++ toString id +++ "-thread"
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
		# (procs,iworld) 	= processStore id iworld
		= case [process \\ process <- procs | process.Process.processId == processId] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)

	getProcessThread :: !ProcessId !*IWorld -> (!Maybe Dynamic,	!*IWorld)
	getProcessThread pid iworld
		= loadValue (THREAD_DB pid) iworld
	
	getProcessContext :: !ProcessId !*IWorld -> (!Maybe TaskContext, !*IWorld)
	getProcessContext pid iworld
		= loadValue (CONTEXT_DB pid) iworld

	getProcessForUser :: !User !ProcessId !*IWorld -> (!Maybe Process,!*IWorld)
	getProcessForUser user processId iworld
		# (procs,iworld) 	= processStore id iworld
		= case [p\\ p <- procs |   p.Process.processId == processId
							   && user == p.Process.properties.ProcessProperties.managerProperties.worker
							      ] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)
				
	getProcesses :: ![TaskStatus] ![RunningTaskStatus] !*IWorld -> (![Process], !*IWorld)
	getProcesses statusses runningStatusses iworld 
		# (procs, iworld)	= processStore id iworld
		= ([p \\ p <- procs | isMember p.Process.properties.systemProperties.SystemProperties.status statusses && isMember p.Process.properties.ProcessProperties.managerProperties.ManagerProperties.status runningStatusses], iworld)
			
	getProcessesById :: ![ProcessId] !*IWorld -> (![Process], !*IWorld)
	getProcessesById ids iworld
		# (procs,iworld) 	= processStore id iworld
		= ([process \\ process <- procs | isMember process.Process.processId ids], iworld)
	
	getProcessesForUser	:: !User ![TaskStatus] ![RunningTaskStatus] !*IWorld -> (![Process], !*IWorld)
	getProcessesForUser user statusses runningStatusses iworld
		# (procs,iworld) 	= processStore id iworld
		= (filterProcs (\p -> isRelevant user p && isMember p.Process.properties.systemProperties.SystemProperties.status statusses && isMember p.Process.properties.ProcessProperties.managerProperties.ManagerProperties.status runningStatusses) procs, iworld)
		//= ([p \\ p <- procs | isRelevant user p && isMember p.Process.properties.systemProperties.SystemProperties.status statusses && isMember p.Process.properties.ProcessProperties.managerProperties.ManagerProperties.status runningStatusses], iworld)
	where
		isRelevant user {Process | properties}	
			//Either you are working on the task
			=  ( properties.ProcessProperties.managerProperties.worker == user)
	
	setProcessThread :: !ProcessId !Dynamic !*IWorld -> *IWorld
	setProcessThread pid thread iworld
		= storeValue (THREAD_DB pid) thread iworld
		
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
		# (procs,iworld) 	= processStore id iworld
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
		# (procs,iworld) 	= processStore id iworld
		# (nprocs,iworld)	= processStore (\_ -> [process \\ process <- procs | process.Process.processId <> processId]) iworld
		= (length procs <> length nprocs, iworld)


processStore ::  !([Process] -> [Process]) !*IWorld -> (![Process],!*IWorld) 
processStore fn iworld
	# (mbList,iworld)	= loadValue "ProcessDB" iworld
	# list 				= fn (case mbList of Nothing = []; Just list = list)
	# iworld			= storeValue "ProcessDB" list iworld 
	= (list,iworld)

contextToProcess :: !ProcessId !TaskContext -> Process
contextToProcess processId (TCTop properties _ scontext)
	= {processId = processId, properties = properties, subprocesses = tsubprocs scontext}
where
	tsubprocs (TTCActive context)			= subprocs context
	tsubprocs _								= []

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
filterProcs pred procs = procs //TODO

 

