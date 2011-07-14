implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Task, TaskContext, WorkflowDB

from CoreCombinators 	import >>=
from CoreCombinators	import :: TaskContainer(..), ::TaskGUI(..), :: ParallelTask(..), :: ParallelControl
from TuningCombinators	import class tune, instance tune Description, <<@, @>>, :: Description(..)
from InteractionTasks	import enterInformation, :: LocalViewOn, :: ViewOn
from ProcessDB	import qualified class ProcessDB(..), instance ProcessDB IWorld
from Map		import qualified fromList, get, put
import iTaskClass

ITERATION_THRESHOLD :== 10

createThread :: (Task a) -> Dynamic | iTask a
createThread task = (dynamic container :: Container (TaskThread a^) a^)
where
 	container = Container {TaskThread|originalTask = task, currentTask = task}

//Creeer initiele task context
makeWorkflowInstance :: !WorkflowId !User !(Maybe JSONNode) !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
makeWorkflowInstance workflowId user mbParam iworld
	# (mbWorkflow,iworld)	= getWorkflow workflowId iworld
	= case mbWorkflow of
		Nothing
			= (Error "No such workflow",iworld)
		Just {Workflow|task,managerProperties}
			# mbThread = case task of
				WorkflowTask task
					| isNothing mbParam	= Ok (createThread task)
					| otherwise			= Error "Workflow has no parameter"
				ParamWorkflowTask paramTask = case mbParam of
					Just param = case fromJSON param of
						Just param		= Ok (createThread (paramTask param))
						Nothing			= Error "Invalid argument"
					Nothing				= Ok (createThread (enterInformation ("Workflow parameter","Enter the parameter of the workflow") [] >>= paramTask))
			= case mbThread of
				Ok thread
					# (processId,iworld)		= 'ProcessDB'.getNextProcessId iworld
					# (context,iworld)			= initTaskContext processId thread managerProperties iworld
					= (Ok context, iworld)
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

//Laadt bestaande context en pas eventuele edit events toe
loadWorkflowInstance :: !TaskNr !(Maybe EditEvent) !*IWorld	-> (!MaybeErrorString TaskContext, !*IWorld)
loadWorkflowInstance taskNr editEvent iworld = (Error "NOT IMPLEMENTED", iworld)
	//Lookup task context
	
	//Apply edit events if applicable

//Evalueer de task in de gegeven context
evalWorkflowInstance :: !TaskNr !TaskContext !(Maybe CommitEvent) !*IWorld	-> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
evalWorkflowInstance taskNr context editEvent iworld = (Error "NOT IMPLEMENTED", iworld)
	//Eval instance
	//Process controls
	
	//Get the target param
	
createWorkflowInstance :: !WorkflowId !User !(Maybe JSONNode) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic,!ProcessProperties), !*IWorld)
createWorkflowInstance workflowId user mbParam iworld 
	//Lookup workflow
	# (mbWorkflow,iworld)	= getWorkflow workflowId iworld
	= case mbWorkflow of
		Nothing
			= (Error "No such workflow",iworld)
		Just {Workflow|task,managerProperties}
			# mbThread = case task of
				WorkflowTask task
					| isNothing mbParam	= Ok (createThread task)
					| otherwise			= Error "Workflow has no parameter"
				ParamWorkflowTask paramTask = case mbParam of
					Just param = case fromJSON param of
						Just param		= Ok (createThread (paramTask param))
						Nothing			= Error "Invalid argument"
					Nothing				= Ok (createThread (enterInformation ("Workflow parameter","Enter the parameter of the workflow") [] >>= paramTask))
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
		# (tcontext,iworld) = (toTaskFuncs originalTask).initFun [0,processId] iworld		
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
evaluateWorkflowInstance processId mbEdit mbCommit tuiTaskNr iworld = evaluateWorkflowInstance` mbEdit mbCommit 1 iworld
where
	evaluateWorkflowInstance` mbEdit mbCommit iterationCount iworld
		//Load task context
		# (mbContext,iworld)			= 'ProcessDB'.getProcessContext processId iworld
		| isNothing mbContext
			= (Error "Could not load task context", iworld)
		//Evaluate
		# (result,iworld=:{readShares})	= evalTask processId mbEdit mbCommit (reverse tuiTaskNr) (fromJust mbContext) iworld
		//Process controls (additions/removals of processes)
		# iworld						= processControls iworld
		= case result of
			TaskBusy _ _ _ | isNothing readShares && iterationCount < ITERATION_THRESHOLD
				= evaluateWorkflowInstance` Nothing Nothing (inc iterationCount) iworld
			_
				= (Ok result, iworld)

	evalTask processId mbEdit mbCommit tuiTaskNr context=:(TaskContext properties changeNo tcontext) iworld=:{IWorld|timestamp}
		//Set current worker & last event timestamp
		# iworld = {iworld & currentUser = properties.ProcessProperties.managerProperties.worker, currentProcess = processId, latestEvent = properties.systemProperties.SystemProperties.latestEvent}
		//Set initial control of global task list
		# iworld = {iworld & parallelControls = 'Map'.fromList [(toString GlobalTaskList,(0,[]))]}
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
				# commitEvent					= stepEvent changeNo (stepEvent processId mbCommit) //TODO: hier switchen van proc naar task event
				= evaluateWorkflowInstanceEval processId currentTask.Task.properties changeNo [changeNo,processId] properties thread scontext commitEvent tuiTaskNr iworld
			TTCFinished result
				= (TaskFinished result, iworld)
			TTCExcepted e
				= (taskException e, iworld)

	processControls iworld
		# (controls,iworld) = getControls iworld
	 	= processControls` controls iworld
	 	
	processControls` [] iworld = iworld
	processControls` [c:cs] iworld=:{currentUser} = case c of
		AppendTask pid (container :: TaskContainer Void)
			//Make thread and properties
			# (thread,managerProperties) = case container of
				(DetachedTask props,tfun)	= (createThread (tfun GlobalTaskList),props)
				(WindowTask _,tfun)			= (createThread (tfun GlobalTaskList),{initManagerProperties & worker = currentUser})
				(DialogTask _,tfun)			= (createThread (tfun GlobalTaskList),{initManagerProperties & worker = currentUser})
				(BodyTask,tfun)				= (createThread (tfun GlobalTaskList),{initManagerProperties & worker = currentUser})
				(HiddenTask,tfun)			= (createThread (tfun GlobalTaskList),{initManagerProperties & worker = currentUser})
			//Make context
			# (context,iworld) = initTaskContext pid thread managerProperties iworld
			//Eval, but ignore the result
			# (_, iworld) 		= evalTask pid Nothing Nothing [pid] context iworld
			//Fetch potential additional controls
			# (moreCs,iworld)	= getControls iworld
			= processControls` (cs ++ moreCs) iworld
		_
			= processControls` cs iworld			
	
	getControls iworld=:{parallelControls}
		= case 'Map'.get (toString GlobalTaskList) parallelControls of
			Just (_,controls)	= (controls, {iworld & parallelControls = 'Map'.put (toString GlobalTaskList)(0,[]) parallelControls})
			_					= ([],iworld)
	
	initTaskContext processId thread=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) managerProperties iworld=:{IWorld|timestamp,currentUser}
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
			, managerProperties	= managerProperties
			}
		= (TaskContext properties 0 (TTCRunning thread tcontext),iworld)

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