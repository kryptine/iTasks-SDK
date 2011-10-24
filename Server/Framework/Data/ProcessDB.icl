implementation module ProcessDB

import StdEnv, Maybe

import IWorld, TaskContext, Store, Util, Text, Time, Random
import SerializationGraphCopy //TODO: Make switchable from within iTasks module
 
derive bimap Maybe, (,)

NEXT_ID_DB		:== "NextProcessID"
PROCESS_DB		:== "ProcessDB"
CONTEXT_DB id	:== "Process-" +++ toString id +++ "-context"

newSessionId :: !*IWorld -> (!ProcessId,!*IWorld)
newSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (SessionProcess (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)])) , {IWorld|iworld & world = world})
	
newWorkflowId :: !*IWorld -> (!ProcessId,!*IWorld)
newWorkflowId iworld
	# (mbNewPid,iworld) = loadValue NEXT_ID_DB iworld
	= case mbNewPid of
		Just pid
			# iworld = storeValue NEXT_ID_DB (pid+1) iworld 
			= (WorkflowProcess pid,iworld)
		Nothing
			# iworld = storeValue NEXT_ID_DB 2 iworld //store the next value (2)
			= (WorkflowProcess 1,iworld) //return the first value (1)		

storeTaskInstance :: !TaskContext !*IWorld -> *IWorld
storeTaskInstance context=:(TaskContext pid _ _ _ _ _) iworld
		//Store the context
		# iworld = storeValue (CONTEXT_DB pid) context iworld
		//Update the process table with the process information from this contexts
		# iworld = snd (processStore (update (contextToInstanceMeta context)) iworld)
		= iworld
	where
		update process [] = [process]
		update process [p:ps] = if (p.processId == process.processId) [process:ps] [p:update process ps]
	
loadTaskInstance :: !ProcessId !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
loadTaskInstance pid iworld
	# (val,iworld) = loadValue (CONTEXT_DB pid) iworld
	= (maybe (Error ("Could not load instance " +++ toString pid)) Ok val, iworld)
	
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
