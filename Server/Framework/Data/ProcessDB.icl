implementation module ProcessDB

import StdEnv, Maybe

import IWorld, TaskContext, Store, Util, Text, Time, Random
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
	getNewSessionId	:: !*IWorld -> (!ProcessId,!*IWorld)
	getNewSessionId iworld
		# (sid,iworld) = genSessionId iworld
		= (SessionProcess sid, iworld)
	
	getNewWorkflowId :: !*IWorld -> (!ProcessId,!*IWorld)
	getNewWorkflowId iworld
		# (mbNewPid,iworld) = loadValue NEXT_ID_DB iworld
		= case mbNewPid of
			Just pid
				# iworld = storeValue NEXT_ID_DB (pid+1) iworld 
				= (WorkflowProcess pid,iworld)
			Nothing
				# iworld = storeValue NEXT_ID_DB 2 iworld //store the next value (2)
				= (WorkflowProcess 1,iworld) //return the first value (1)		
	
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
							   && maybe False (\u -> u == user) p.Process.properties.ProcessProperties.managerProperties.worker
							      ] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)
				
	getProcesses :: ![TaskStatus] !*IWorld -> (![Process], !*IWorld)
	getProcesses statusses iworld 
		# (procs, iworld)	= readProcessStore iworld
		= (filterProcs (\p -> isMember p.Process.properties.systemProperties.SystemProperties.status statusses) procs, iworld)
			
	getProcessesById :: ![ProcessId] !*IWorld -> (![Process], !*IWorld)
	getProcessesById ids iworld
		# (procs,iworld) 	= readProcessStore iworld
		= ([process \\ process <- procs | isMember process.Process.processId ids], iworld)
	
	getProcessesForUser	:: !User ![TaskStatus] !*IWorld -> (![Process], !*IWorld)
	getProcessesForUser user statusses iworld
		# (procs,iworld) 	= readProcessStore iworld
		= (filterProcs (\p -> isRelevant user p && isMember p.Process.properties.systemProperties.SystemProperties.status statusses) procs, iworld)
	where
		isRelevant user {Process | properties}	
			//Either you are working on the task
			= maybe False (\u -> u == user) properties.ProcessProperties.managerProperties.worker
		
	setProcessContext :: !ProcessId !TaskContext !*IWorld -> *IWorld
	setProcessContext processId context iworld
		//Store the context
		# iworld = storeValue (CONTEXT_DB processId) context iworld
		//Update the process table with the process information from this contexts
		# iworld = snd (processStore (update (contextToProcess context)) iworld)
		= iworld
	where
		update process [] = [process]
		update process [p:ps] = if (p.processId == process.processId) [process:ps] [p:update process ps]

	deleteProcess :: !ProcessId !*IWorld -> *IWorld
	deleteProcess processId iworld 
		//Delete values from store
		# iworld = deleteValues ("Process-" +++ toString processId) iworld
		//Delete from process table
		# (_,iworld)	= processStore (\procs -> [process \\ process <- procs | process.Process.processId <> processId]) iworld
		= iworld
		
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

genSessionId :: !*IWorld -> (!String, !*IWorld)
genSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) , {IWorld|iworld & world = world})

contextToProcess :: !TaskContext -> Process
contextToProcess (TaskContext processId properties _ scontext)
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
		= [{processId = addTarget properties.systemProperties.taskId processId
		   ,properties = properties
		   ,subprocesses = case context of Nothing = []; Just (_,c) = subprocs c}
		  :subprocsp subs]
	subprocsp [(_,STCEmbedded _ Nothing):subs]			= subprocsp subs
	subprocsp [(_,STCEmbedded _ (Just (_,c))):subs]		= subprocs c ++ subprocsp subs
	subprocsp [(_,STCHidden _ Nothing):subs]		= subprocsp subs
	subprocsp [(_,STCHidden _ (Just (_,c))):subs]	= subprocs c ++ subprocsp subs
	
	addTarget target (WorkflowProcess pid) = (EmbeddedProcess pid target)
	addTarget _ procId = procId
					 
filterProcs :: (Process -> Bool) [Process] -> [Process]
filterProcs pred procs = flatten [if (pred p) [{p & subprocesses = filterProcs pred p.subprocesses}] (filterProcs pred p.subprocesses) \\  p <- procs]
