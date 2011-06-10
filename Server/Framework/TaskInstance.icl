implementation module TaskInstance

import StdList, StdBool
import Error
import SystemTypes, IWorld, Task, TaskContext, WorkflowDB, ProcessDB

from CoreCombinators import >>=
from TuningCombinators import class tune, instance tune Description, <<@, @>>, :: Description(..)
from InteractionTasks import enterInformation, :: LocalViewOn, :: ViewOn
import iTaskClass

ITERATION_THRESHOLD :== 10

createThread :: (Task a) -> Dynamic | iTask a
createThread task = (dynamic container :: Container (TaskThread a^) a^)
where
 	container = Container {TaskThread|originalTask = task, currentTask = task}
 	
createThreadParam :: !String (a -> Task b)	-> Dynamic | iTask a & iTask b
createThreadParam title task = (dynamic container :: Container (Container (TaskThreadParam a^ b^) b^) a^)
where
 	container = Container (Container ({TaskThreadParam|originalTask = task, currentTask = task, title = title}))

toNonParamThreadValue :: !String !Dynamic -> Maybe Dynamic
toNonParamThreadValue vStr (Container (Container {TaskThreadParam|originalTask,currentTask,title}) :: Container (Container (TaskThreadParam a b) b) a)
	= case fromJSON (fromString vStr) of
		Just v = 
			Just (dynamic Container {TaskThread | originalTask = originalTask v <<@ Description title, currentTask = currentTask v <<@ Description title} :: Container (TaskThread b) b)
		Nothing =
			Nothing
toNonParamThreadValue _ _ = Nothing

toNonParamThreadEnter :: !Dynamic -> Dynamic
toNonParamThreadEnter (Container (Container {TaskThreadParam|originalTask,currentTask,title}) :: Container (Container (TaskThreadParam a b) b) a)
	= (dynamic Container {TaskThread | originalTask = enterParam originalTask, currentTask = enterParam currentTask} :: Container (TaskThread b) b)
where		
	enterParam paramTask = Description title @>> (enterInformation ("Workflow parameter","Enter the parameter of the workflow") [] >>= paramTask)


createWorkflowInstance :: !WorkflowId !User !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic,!ProcessProperties), !*IWorld)
createWorkflowInstance workflowId user iworld 
	//Lookup workflow
	# (mbWorkflow,iworld)	= getWorkflow workflowId iworld
	= case mbWorkflow of
		Nothing
			= (Error "No such workflow",iworld)
		Just {Workflow|thread,managerProperties}
			//Get next process id
			# (processId,iworld)		= getNextProcessId iworld
			//Create initial task context
			# (context,iworld)			= initTaskContext processId thread managerProperties iworld
			//Store thread
			# iworld					= setProcessThread processId thread iworld
			//Evaluate task once
			# (result,properties,iworld)= evalTask processId thread context iworld 
			= (Ok (result,properties), iworld)
where
	initTaskContext processId thread=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) managerProperties=:{worker} iworld=:{IWorld|timestamp}
		# originalTaskFuncs = toTaskFuncs originalTask
		# (tcontext,iworld) = originalTaskFuncs.initFun [0,processId] iworld		
		# properties =
			{ taskProperties	= taskProperties originalTask
			, systemProperties	=
				{ taskId		= toString processId
				, status		= Running
				, issuedAt		= timestamp
				, firstEvent	= Nothing
				, latestEvent	= Nothing
				}
			, managerProperties	= {managerProperties & worker = if (worker == AnyUser) user worker}
			}
		= (TCTop properties 0 (TTCActive tcontext),iworld)

	evalTask processId thread=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) (TCTop properties changeNo (TTCActive tcontext)) iworld
		# originalTaskFuncs = toTaskFuncs originalTask
		//Set current worker
		# iworld = {iworld & currentUser = properties.ProcessProperties.managerProperties.worker}
		//Evaluate
		# (tresult, iworld)	= originalTaskFuncs.evalTaskFun [changeNo,processId] originalTask.Task.properties Nothing [] defaultInteractionLayout defaultParallelLayout defaultMainLayout tcontext iworld
		= case tresult of
			TaskBusy tui actions tcontext
				# properties	= setRunning properties
				# tui			= defaultMainLayout {TUIMain|content = fromJust tui,actions = actions, properties = properties} 
				# context 		= TCTop properties changeNo (TTCActive tcontext)
				# iworld		= setProcessContext processId context iworld
				= (TaskBusy (Just tui) [] context, properties, iworld)
			TaskFinished val	
				# properties	= setFinished properties
				# context		= TCTop properties  changeNo (TTCFinished (toJSON val))
				# iworld		= setProcessContext processId context iworld
				= (TaskFinished (dynamic val :: a), properties, iworld)
			TaskException e str
				# properties	= setExcepted properties
				# context		= TCTop properties changeNo (TTCExcepted str)
				# iworld		= setProcessContext processId context iworld
				= (TaskException e str, properties, iworld)

evaluateWorkflowInstance :: !ProcessId !(Maybe EditEvent) !(Maybe CommitEvent) !TaskNr !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic,!ProcessProperties), !*IWorld)
evaluateWorkflowInstance processId mbEdit mbCommit tuiTaskNr iworld
	//Load thread
	# (mbThread,iworld)		= getProcessThread processId iworld
	| isNothing mbThread
		= (Error "Could not load task definition", iworld)
	//Load task context
	# (mbContext,iworld)	= getProcessContext processId iworld
	| isNothing mbThread
		= (Error "Could not load task context", iworld)
	//Evaluate
	# (result,properties,iworld) = evalTask processId mbEdit mbCommit (reverse tuiTaskNr) (fromJust mbThread) (fromJust mbContext) iworld
	= (Ok (result,properties), iworld)
where
	evalTask processId mbEdit mbCommit tuiTaskNr thread=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) context=:(TCTop properties changeNo tcontext) iworld=:{IWorld|timestamp}
		# originalTaskFuncs = toTaskFuncs originalTask
		//Set current worker & last event timestamp
		# iworld = {iworld & currentUser = properties.ProcessProperties.managerProperties.worker, latestEvent = properties.systemProperties.SystemProperties.latestEvent}
		//If target is not detached process, set first event timestamp if not set yet & latest update
		# properties = case tuiTaskNr of
			[_]	= {properties & systemProperties = {properties.systemProperties & firstEvent = Just (fromMaybe timestamp properties.systemProperties.firstEvent), latestEvent = Just timestamp}}
			_	= properties
		//Remove first parts of tui task number
		# tuiTaskNr			= stepTUITaskNr changeNo (stepTUITaskNr processId tuiTaskNr)
		= case tcontext of
			//Evaluate further
			TTCActive scontext
				//Apply edit event
				# (scontext,iworld) = case mbEdit of
					//The firts two steps in an edit event path have to be the processId and changeNo
					Just ([processId,changeNo:steps],path,val)	= originalTaskFuncs.editEventFun [changeNo,processId] (steps,path,val) scontext iworld
					_											= (scontext, iworld)
				//Evaluate
				//The first two steps in a commit event path have to be the processId and changeNo
				# commitEvent					= stepCommitEvent changeNo (stepCommitEvent processId mbCommit)
				= evalTask` originalTask.Task.properties changeNo scontext commitEvent tuiTaskNr originalTaskFuncs properties 1 iworld
			//Don't evaluate, just yield TaskBusy without user interface and original context
			TTCSuspended scontext
				= (TaskBusy Nothing [] context, properties, iworld)
			TTCFinished encval
				= case fromJSON encval of
					Just val	= (TaskFinished (dynamic val :: a), properties, iworld)
					Nothing		= (taskException "Could not decode result", properties, iworld)
			TTCExcepted e
				= (taskException e, properties, iworld)
			
	evalTask` props changeNo scontext commitEvent tuiTaskNr originalTaskFuncs properties iterationCount iworld
		# (sresult,iworld)	= originalTaskFuncs.evalTaskFun [changeNo,processId] props commitEvent tuiTaskNr defaultInteractionLayout defaultParallelLayout defaultMainLayout scontext {iworld & readShares = Just []}
		= case sresult of
			TaskBusy tui actions scontext
				# properties	= setRunning properties 
				# tui			= if (isEmpty tuiTaskNr)
					(fmap (\t -> defaultMainLayout {TUIMain|content = t,actions = actions, properties = properties}) tui)
					tui
				# context		= TCTop properties changeNo (TTCActive scontext)
				# iworld		= setProcessContext processId context iworld
				| isNothing iworld.readShares && iterationCount < ITERATION_THRESHOLD
					= evalTask` props changeNo scontext Nothing tuiTaskNr originalTaskFuncs properties (inc iterationCount) iworld
				| otherwise
					= (TaskBusy tui [] context, properties, iworld)
			TaskFinished val
				# properties	= setFinished properties
				# context		= TCTop properties changeNo (TTCFinished (toJSON val))
				# iworld		= setProcessContext processId context iworld
				= (TaskFinished (dynamic val :: a^), properties, iworld)
			TaskException _ e
				# properties	= setExcepted properties
				# context		= TCTop properties changeNo (TTCExcepted e)
				# iworld		= setProcessContext processId context iworld
				= (taskException e, properties, iworld)

setRunning properties=:{systemProperties} = {properties & systemProperties = {SystemProperties|systemProperties & status = Running}}
setFinished properties=:{systemProperties} = {properties & systemProperties = {SystemProperties|systemProperties & status = Finished}}
setExcepted properties=:{systemProperties} = {properties & systemProperties = {SystemProperties|systemProperties & status = Excepted}}
