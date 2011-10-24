implementation module ProcessDB

import StdEnv, Maybe

import IWorld, TaskContext, Store, Util, Text, Time, Random
import SerializationGraphCopy //TODO: Make switchable from within iTasks module
 
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
	
	getProcess :: !ProcessId !*IWorld -> (!Maybe TaskInstanceMeta,!*IWorld)
	getProcess processId iworld
		# (procs,iworld) 	= readProcessStore iworld
		= case [process \\ process <- procs | process.TaskInstanceMeta.processId == processId] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)
	
	getProcessContext :: !ProcessId !*IWorld -> (!Maybe TaskContext, !*IWorld)
	getProcessContext pid iworld
		= loadValue (CONTEXT_DB pid) iworld

	getProcessForUser :: !User !ProcessId !*IWorld -> (!Maybe TaskInstanceMeta,!*IWorld)
	getProcessForUser user processId iworld
		# (procs,iworld) 	= readProcessStore iworld
		= case [p\\ p <- procs |   p.TaskInstanceMeta.processId == processId
							   && maybe False (\u -> u == user) p.TaskInstanceMeta.managementMeta.worker
							      ] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)
				
	getProcesses :: ![TaskStatus] !*IWorld -> (![TaskInstanceMeta], !*IWorld)
	getProcesses statusses iworld 
		# (procs, iworld)	= readProcessStore iworld
		= (filterProcs (\p -> isMember p.TaskInstanceMeta.progressMeta.status statusses) procs, iworld)
			
	getProcessesById :: ![ProcessId] !*IWorld -> (![TaskInstanceMeta], !*IWorld)
	getProcessesById ids iworld
		# (procs,iworld) 	= readProcessStore iworld
		= ([process \\ process <- procs | isMember process.TaskInstanceMeta.processId ids], iworld)
	
	getProcessesForUser	:: !User ![TaskStatus] !*IWorld -> (![TaskInstanceMeta], !*IWorld)
	getProcessesForUser user statusses iworld
		# (procs,iworld) 	= readProcessStore iworld
		= (filterProcs (\p -> isRelevant user p && isMember p.TaskInstanceMeta.progressMeta.status statusses) procs, iworld)
	where
		isRelevant user proc	
			//Either you are working on the task
			= maybe False (\u -> u == user) proc.TaskInstanceMeta.managementMeta.worker
		
	setProcessContext :: !ProcessId !TaskContext !*IWorld -> *IWorld
	setProcessContext processId context iworld
		//Store the context
		# iworld = storeValue (CONTEXT_DB processId) context iworld
		//Update the process table with the process information from this contexts
		# iworld = snd (processStore (update (contextToInstanceMeta context)) iworld)
		= iworld
	where
		update process [] = [process]
		update process [p:ps] = if (p.processId == process.processId) [process:ps] [p:update process ps]

	deleteProcess :: !ProcessId !*IWorld -> *IWorld
	deleteProcess processId iworld 
		//Delete values from store
		# iworld = deleteValues ("Process-" +++ toString processId) iworld
		//Delete from process table
		# (_,iworld)	= processStore (\procs -> [process \\ process <- procs | process.TaskInstanceMeta.processId <> processId]) iworld
		= iworld
		
	lastChange :: !*IWorld -> (!Timestamp,!*IWorld)
	lastChange iworld
		# (mbTs,iworld) = getStoreTimestamp PROCESS_DB iworld
		= (fromMaybe (Timestamp 0) mbTs,iworld)


processStore ::  !([TaskInstanceMeta] -> [TaskInstanceMeta]) !*IWorld -> (![TaskInstanceMeta],!*IWorld) 
processStore fn iworld
	# (list,iworld)		= readProcessStore iworld
	# list 				= fn list
	# iworld			= storeValue "ProcessDB" list iworld 
	= (list,iworld)
	
readProcessStore :: !*IWorld -> (![TaskInstanceMeta],!*IWorld) 
readProcessStore iworld
	# (mbList,iworld)	= loadValue "ProcessDB" iworld
	= (fromMaybe [] mbList,iworld)

genSessionId :: !*IWorld -> (!String, !*IWorld)
genSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) , {IWorld|iworld & world = world})

contextToInstanceMeta :: !TaskContext -> TaskInstanceMeta
contextToInstanceMeta (TaskContext processId tmeta pmeta mmeta _ scontext)
	= {processId = processId, taskMeta = tmeta, progressMeta = pmeta, managementMeta = mmeta, subInstances = tsubprocs scontext}
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
	subprocsp [(_,STCDetached tmeta pmeta mmeta context):subs]
		= [{processId = addTarget "TODO taskId" processId
		   ,taskMeta = tmeta
		   ,progressMeta = pmeta
		   ,managementMeta = mmeta
		   ,subInstances = case context of Nothing = []; Just (_,c) = subprocs c}
		  :subprocsp subs]
	subprocsp [(_,STCEmbedded _ Nothing):subs]			= subprocsp subs
	subprocsp [(_,STCEmbedded _ (Just (_,c))):subs]		= subprocs c ++ subprocsp subs
	subprocsp [(_,STCHidden _ Nothing):subs]		= subprocsp subs
	subprocsp [(_,STCHidden _ (Just (_,c))):subs]	= subprocs c ++ subprocsp subs
	
	addTarget target (WorkflowProcess pid) = (EmbeddedProcess pid target)
	addTarget _ procId = procId
					 
filterProcs :: (TaskInstanceMeta -> Bool) [TaskInstanceMeta] -> [TaskInstanceMeta]
filterProcs pred procs = flatten [if (pred p) [{p & subInstances = filterProcs pred p.subInstances}] (filterProcs pred p.subInstances) \\  p <- procs]
