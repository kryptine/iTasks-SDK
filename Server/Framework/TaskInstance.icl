implementation module TaskInstance

import StdList, StdBool
import Error
import SystemTypes, IWorld, Task, TaskContext, WorkflowDB

from CoreCombinators import >>=
from TuningCombinators import class tune, instance tune Description, <<@, @>>, :: Description(..)
from InteractionTasks import enterInformation, :: LocalViewOn, :: ViewOn
from ProcessDB	import qualified class ProcessDB(..), instance ProcessDB IWorld
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

toNonParamThreadValue :: !JSONNode !Dynamic -> Maybe Dynamic
toNonParamThreadValue jsonV (Container (Container {TaskThreadParam|originalTask,currentTask,title}) :: Container (Container (TaskThreadParam a b) b) a)
	= case fromJSON jsonV of
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


createWorkflowInstance :: !WorkflowId !User !(Maybe JSONNode) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic,!ProcessProperties), !*IWorld)
createWorkflowInstance workflowId user mbParam iworld 
	//Lookup workflow
	# (mbWorkflow,iworld)	= getWorkflow workflowId iworld
	= case mbWorkflow of
		Nothing
			= (Error "No such workflow",iworld)
		Just {Workflow|thread,managerProperties}
			# mbThread = case thread of
				(_ :: Container (TaskThread a) a)
					| isNothing mbParam	= Ok thread
					| otherwise			= Error "Workflow has no parameter"
				_ = case mbParam of
					Just param = case toNonParamThreadValue param thread of
						Just thread		= Ok thread
						Nothing			= Error "Invalid argument"
						
					Nothing				= Ok (toNonParamThreadEnter thread)
			= case mbThread of
				Ok thread
					//Get next process id
					# (processId,iworld)		= 'ProcessDB'.getNextProcessId iworld
					//Create initial task context
					# (context,iworld)			= initTaskContext processId thread managerProperties iworld
					//Evaluate task once
					# (result,properties,iworld)= evalTask processId thread context iworld 
					= (Ok (result,properties), iworld)
				Error err
					= (Error err,iworld)
			
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
		= (TaskContext properties 0 (TTCRunning thread tcontext),iworld)

	evalTask processId t=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) (TaskContext properties changeNo (TTCRunning thread tcontext)) iworld
		# originalTaskFuncs = toTaskFuncs originalTask
		//Set current worker
		# iworld = {iworld & currentUser = properties.ProcessProperties.managerProperties.worker, currentProcess = processId}
		//Evaluate
		# (tresult, iworld)	= originalTaskFuncs.evalTaskFun [changeNo,processId] originalTask.Task.properties Nothing [] defaultInteractionLayout defaultParallelLayout defaultMainLayout tcontext iworld
		= case tresult of
			TaskBusy tui actions tcontext
				# properties	= setRunning properties
				# tui			= defaultMainLayout {TUIMain|content = fromJust tui,actions = actions, properties = properties} 
				# context 		= TaskContext properties changeNo (TTCRunning thread tcontext)
				# iworld		= 'ProcessDB'.setProcessContext processId context iworld
				= (TaskBusy (Just tui) [] tcontext, properties, iworld)
			TaskFinished val	
				# properties	= setFinished properties
				# context		= TaskContext properties  changeNo (TTCFinished (dynamic val))
				# iworld		= 'ProcessDB'.setProcessContext processId context iworld
				= (TaskFinished (dynamic val :: a), properties, iworld)
			TaskException e str
				# properties	= setExcepted properties
				# context		= TaskContext properties changeNo (TTCExcepted str)
				# iworld		= 'ProcessDB'.setProcessContext processId context iworld
				= (TaskException e str, properties, iworld)

evaluateWorkflowInstance :: !ProcessId !(Maybe EditEvent) !(Maybe CommitEvent) !TaskNr !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
evaluateWorkflowInstance processId mbEdit mbCommit tuiTaskNr iworld
	//Load task context
	# (mbContext,iworld)	= 'ProcessDB'.getProcessContext processId iworld
	| isNothing mbContext
		= (Error "Could not load task context", iworld)
	//Evaluate
	# (result,iworld) = evalTask processId mbEdit mbCommit (reverse tuiTaskNr) (fromJust mbContext) iworld
	= (Ok result, iworld)
where
	evalTask processId mbEdit mbCommit tuiTaskNr context=:(TaskContext properties changeNo tcontext) iworld=:{IWorld|timestamp}
		//Set current worker & last event timestamp
		# iworld = {iworld & currentUser = properties.ProcessProperties.managerProperties.worker, currentProcess = processId, latestEvent = properties.systemProperties.SystemProperties.latestEvent}
		//If target is not detached process, set first event timestamp if not set yet & latest update
		# properties = case tuiTaskNr of
			[_]	= {properties & systemProperties = {properties.systemProperties & firstEvent = Just (fromMaybe timestamp properties.systemProperties.firstEvent), latestEvent = Just timestamp}}
			_	= properties
		//Remove first parts of tui task number
		# tuiTaskNr			= stepTUITaskNr changeNo (stepTUITaskNr processId tuiTaskNr)
		= case tcontext of
			//Evaluate further
			TTCRunning thread=:(Container {TaskThread|currentTask} :: Container (TaskThread a) a) scontext
				# taskFuncs = toTaskFuncs currentTask
				//Apply edit event
				# (scontext,iworld) = case mbEdit of
					//The firts two steps in an edit event path have to be the processId and changeNo
					Just ([processId,changeNo:steps],path,val)	= taskFuncs.editEventFun [changeNo,processId] (steps,path,val) scontext iworld
					_											= (scontext, iworld)
				//Evaluate
				//The first two steps in a commit event path have to be the processId and changeNo
				# commitEvent					= stepCommitEvent changeNo (stepCommitEvent processId mbCommit)
				= evalTask` currentTask.Task.properties changeNo scontext commitEvent tuiTaskNr thread properties 1 iworld
			TTCFinished result
				= (TaskFinished result, iworld)
			TTCExcepted e
				= (taskException e, iworld)		
			
	evalTask` props changeNo scontext commitEvent tuiTaskNr thread properties iterationCount iworld
		# (res,iworld) = evaluateWorkflowInstanceEval processId props changeNo [changeNo,processId] properties thread scontext commitEvent tuiTaskNr iworld
		= case res of
			TaskBusy _ _ scontext | isNothing iworld.readShares && iterationCount < ITERATION_THRESHOLD
				= evalTask` props changeNo scontext Nothing tuiTaskNr thread properties (inc iterationCount) iworld
			_
				= (res, iworld)

evaluateWorkflowInstanceEval :: !ProcessId !TaskProperties !Int !TaskNr !ProcessProperties !Dynamic !TaskContextTree !(Maybe CommitEvent) !TaskNr !*IWorld -> (!TaskResult Dynamic, !*IWorld)
evaluateWorkflowInstanceEval processId props changeNo taskNr properties thread=:(Container {TaskThread|currentTask} :: Container (TaskThread a) a) scontext commitEvent tuiTaskNr iworld
	# evalTaskFun		= (toTaskFuncs currentTask).evalTaskFun
	# (sresult,iworld)	= evalTaskFun taskNr props commitEvent tuiTaskNr defaultInteractionLayout defaultParallelLayout defaultMainLayout scontext {iworld & readShares = Just []}
	= case sresult of
		TaskBusy tui actions scontext
			# properties	= setRunning properties 
			# tui			= if (isEmpty tuiTaskNr)
				(fmap (\t -> defaultMainLayout {TUIMain|content = t,actions = actions, properties = properties}) tui)
				tui
			# context		= TaskContext properties changeNo (TTCRunning thread scontext)
			# iworld		= 'ProcessDB'.setProcessContext processId context iworld
			= (TaskBusy tui [] scontext, iworld)
		TaskFinished val
			# properties	= setFinished properties
			# context		= TaskContext properties changeNo (TTCFinished (dynamic val))
			# iworld		= 'ProcessDB'.setProcessContext processId context iworld
			= (TaskFinished (dynamic val :: a), iworld)
		TaskException _ e
			# properties	= setExcepted properties
			# context		= TaskContext properties changeNo (TTCExcepted e)
			# iworld		= 'ProcessDB'.setProcessContext processId context iworld
			= (taskException e, iworld)