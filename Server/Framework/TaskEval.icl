implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Task, TaskContext, WorkflowDB

from CoreCombinators 	import >>=
from CoreCombinators	import :: TaskContainer(..), ::TaskGUI(..), :: ParallelTask(..), :: ParallelControl
from TuningCombinators	import class tune, instance tune Description, <<@, @>>, :: Description(..)
from InteractionTasks	import enterInformation, :: LocalViewOn, :: ViewOn
from SystemTypes		import :: ProcessId(..)

from ProcessDB	import qualified class ProcessDB(..), instance ProcessDB IWorld
from Map		import qualified fromList, get, put
import iTaskClass

from WorkflowAdmin import :: Workflow(..), :: WorkflowTaskContainer(..)

ITERATION_THRESHOLD :== 10

createThread :: (Task a) -> Dynamic | iTask a
createThread task = (dynamic container :: Container (TaskThread a^) a^)
where
 	container = Container {TaskThread|originalTask = task, currentTask = task}

createContext :: !ProcessId !Dynamic !ManagerProperties !*IWorld -> (!TaskContext, !*IWorld)
createContext processId thread=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) managerProperties iworld=:{IWorld|timestamp}
	# originalTaskFuncs = toTaskFuncs originalTask
	# (tcontext,iworld) = originalTaskFuncs.initFun (taskNr processId) iworld		
	# properties =
		{ taskProperties	= taskProperties originalTask
		, systemProperties	=
			{ taskId		= taskNrToString (taskNr processId)
			, status		= Running
			, issuedAt		= timestamp
			, firstEvent	= Nothing
			, latestEvent	= Nothing
			}
		, managerProperties	= managerProperties
		}
	= (TaskContext processId properties 0 (TTCRunning thread tcontext),iworld)
where
	taskNr (WorkflowProcess pid)= [0,pid]
	taskNr (SessionProcess _)	= [0,0]
	
createInstanceFrom :: !WorkflowId !User !(Maybe JSONNode) !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
createInstanceFrom workflowId worker mbParam iworld
	= case (getWorkflow workflowId iworld) of
		(Nothing,iworld)
			= (Error "No such workflow", iworld)
		(Just {Workflow|task,managerProperties}, iworld)
			= case task of
				WorkflowTask normalTask
					| isNothing mbParam	= createWorkflowContext worker normalTask iworld
					| otherwise			= (Error "Workflow has no parameter", iworld)
				ParamWorkflowTask paramTask = case mbParam of
					Just param = case fromJSON param of
						Just param		= createWorkflowContext worker (paramTask param) iworld
						Nothing			= (Error "Invalid argument", iworld)
					Nothing				= createWorkflowContext worker (enterInformation ("Workflow parameter","Enter the parameter of the workflow") [] >>= paramTask) iworld

createWorkflowContext :: !User !(Task a) !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld) | iTask a
createWorkflowContext worker task iworld
	# (processId,iworld)	= 'ProcessDB'.getNewWorkflowId iworld
	# (context,iworld)	 	= createContext processId (createThread task) {initManagerProperties & worker = worker} iworld
	= (Ok context, iworld)

//Load an existing task context
loadInstance :: !ProcessId !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
loadInstance processId iworld
	# (mbContext,iworld) = 'ProcessDB'.getProcessContext processId iworld
	= case mbContext of
		Just context 	= (Ok context, iworld)
		Nothing			= (Error ("Could not load task context for task " +++ toString processId), iworld)

editInstance :: !(Maybe EditEvent) !TaskContext !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
editInstance editEvent context=:(TaskContext processId properties changeNo tcontext) iworld
	= case tcontext of
		TTCRunning thread=:(Container {TaskThread|currentTask} :: Container (TaskThread a) a) scontext
			# editFun			= (toTaskFuncs currentTask).editFun
			# procId			= last (taskNrFromString properties.systemProperties.SystemProperties.taskId)
			# taskNr			= [changeNo,procId]
			# (scontext,iworld) = case editEvent of
				Just (ProcessEvent [p,n:steps] event)
					| p == procId && n == changeNo
						= editFun taskNr (TaskEvent steps event) scontext iworld
					| otherwise	
						= editFun taskNr (ProcessEvent [p,n:steps] event) scontext iworld
				Just (ProcessEvent steps event)
					= editFun taskNr (ProcessEvent steps event) scontext iworld
				_
					= (scontext, iworld)
			= (Ok (TaskContext processId properties changeNo (TTCRunning thread scontext)), iworld)
		_
			= (Ok context, iworld)

//Evaluate the given context and yield the result of the main task indicated by target
evalInstance :: !TaskNr !(Maybe CommitEvent) !TaskContext  !*IWorld	-> (!MaybeErrorString (TaskResult Dynamic), !TaskContext, !*IWorld)
evalInstance target commitEvent context=:(TaskContext processId properties changeNo tcontext) iworld=:{evalStack}
	= case tcontext of
		//Eval instance
		TTCRunning thread=:(Container {TaskThread|currentTask} :: Container (TaskThread a) a) scontext
			# evalFun			= (toTaskFuncs currentTask).evalFun
			# pid				= last (taskNrFromString properties.systemProperties.SystemProperties.taskId)
			# taskNr			= [changeNo,pid]
			# procId			= WorkflowProcess pid
			# taskProperties	= currentTask.Task.properties
			//Update current process id & eval stack in iworld
			# iworld			= {iworld & evalStack = [procId:evalStack]} 
			//Strip the process id and change number from the commit event and switch from ProcessEvent to TaskEvent if it matches
			# commitEvent = case commitEvent of
					Just (ProcessEvent [p,n:steps] action)
						| p == pid && n == changeNo			= Just (TaskEvent steps action)
						| otherwise							= Just (ProcessEvent [p,n:steps] action)
					Just (ProcessEvent steps action)		= Just (ProcessEvent steps action)
					_										= Nothing
			//Match processId & changeNo in target path
			# target			= foldr stepTarget [changeNo,pid] target
			//Apply task's eval function	
			# (result,iworld)	= evalFun taskNr taskProperties commitEvent target defaultInteractionLayout defaultParallelLayout scontext iworld 
			//Restore current process id in iworld
			# iworld			= {iworld & evalStack = evalStack}
			= case result of
				TaskBusy tui actions scontext
				//	# properties	= setRunning properties
					# context		= TaskContext processId (setRunning properties) changeNo (TTCRunning thread scontext)
					= (Ok (TaskBusy tui actions scontext), context, iworld)
				TaskFinished val
					# context		= TaskContext processId (setFinished properties) changeNo (TTCFinished (dynamic val))
					= (Ok (TaskFinished (dynamic val)), context, iworld)
				TaskException e str
					# context		= TaskContext processId (setExcepted properties) changeNo (TTCExcepted str)
					= (Ok (TaskException e str), context, iworld)
		TTCFinished r
			= (Ok (TaskFinished r), context, iworld)
		TTCExcepted e
			= (Ok (taskException e), context, iworld)
			
storeInstance :: !TaskContext !*IWorld -> *IWorld
storeInstance context=:(TaskContext processId properties changeNo _) iworld
	= 'ProcessDB'.setProcessContext processId context iworld

createTopInstance :: !Int !User !(Maybe JSONNode) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !TaskNr), !*IWorld)
createTopInstance workflowId user mbParam iworld
	# iworld				= {iworld & currentUser = user}
	# (mbContext,iworld)	= createInstanceFrom workflowId user mbParam iworld
	= case mbContext of
		Error e				= (Error e, iworld)
		Ok context	
			# (mbRes,iworld) = iterateEval [] Nothing context iworld
			= case mbRes of
				Ok result	= (Ok (result, procNo context), iworld)
				Error e		= (Error e, iworld)
where
	procNo (TaskContext (WorkflowProcess pid) properties _ _) = [pid]

createSessionInstance :: !(Task a) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !ProcessId), !*IWorld) |  iTask a
createSessionInstance task iworld
	# (sessionId,iworld)	= 'ProcessDB'.getNewSessionId iworld
	# (context, iworld)		= createContext sessionId (createThread task) initManagerProperties iworld
	# (mbRes,iworld)		= iterateEval [0,0] Nothing context iworld
	= case mbRes of
		Ok result	= (Ok (result, sessionId), iworld)
		Error e		= (Error e, iworld)

evalTopInstance :: !TaskNr !User !(Maybe EditEvent) !(Maybe CommitEvent) !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
evalTopInstance target user editEvent commitEvent iworld
	# iworld				= {iworld & currentUser = user}
	# (mbContext,iworld) 	= loadInstance processId iworld
	= case mbContext of
		Error e				= (Error e, iworld)
		Ok context
			//Apply edit event (only once)
			# (mbContext, iworld) = editInstance editEvent context iworld
			= case mbContext of
				Error e				= (Error e, iworld)
				Ok context			= iterateEval (addChangeNo target context) commitEvent context iworld
where
	//Adds the current changenumber to the target if only a process id is given
	addChangeNo [processId] (TaskContext _ _ changeNo _)	= [changeNo,processId]
	addChangeNo target _									= target	

	processId = WorkflowProcess (last target)

evalSessionInstance :: !ProcessId !(Maybe EditEvent) !(Maybe CommitEvent) !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
evalSessionInstance sessionId editEvent commitEvent iworld
	# (mbContext,iworld)	= loadInstance sessionId iworld
	= case mbContext of
		Error e				= (Error e, iworld)
		Ok context
			//Apply edit event (only once)
			# (mbContext, iworld) = editInstance editEvent context iworld
			= case mbContext of
				Error e				= (Error e, iworld)
				Ok context			= iterateEval [0,0] commitEvent context iworld
	
iterateEval :: !TaskNr !(Maybe CommitEvent) !TaskContext !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
iterateEval target commitEvent context iworld = eval target commitEvent 1 context iworld
where
	eval :: !TaskNr !(Maybe CommitEvent) !Int !TaskContext !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
	eval target commitEvent iteration context iworld
		//Initialize the toplevel task list
		# iworld 	= {iworld & parallelControls = 'Map'.fromList [(toString GlobalTaskList,(0,[]))]}
		//Reset read shares list
		# iworld			= {iworld & readShares = Just []} 
		//Evaluate top instance	
		# (mbResult, context, iworld) = evalInstance target commitEvent context iworld 
		= case mbResult of
			Error e
				= (Error e, iworld)
			Ok result
				//Process controls (eval & store additions/removals)
				# iworld				= processControls iworld
				//Check for next iteration counter: save or re-evaluate
				# iworld=:{readShares}	= iworld
				= case result of
					(TaskBusy _ _ _)
						| isNothing readShares && iteration < ITERATION_THRESHOLD
							= eval target Nothing (iteration + 1) context iworld
						| otherwise
							# iworld	= storeInstance context iworld
							= (Ok result,iworld)
					_
						# iworld	= storeInstance context iworld
						= (Ok result,iworld)
		
	processControls :: !*IWorld -> *IWorld
	processControls iworld = processControls` [] iworld
	where	
		processControls` queue iworld
			//Check for additions/removals in the toplevel task list
			# (controls, iworld)	= getControls iworld
			//Execute the additions/removals. Additions are appended to the queue to be evaluated
			# (queue, iworld)		= execControls controls queue iworld
			//Eval an element of the queue and recurse
			= case queue of 
				[]		= iworld
				[c:cs]
					//Evaluate and store the context only. We do not want any result	
					# (_,c,iworld)	= evalInstance (topTarget c) Nothing c iworld
					# iworld		= storeInstance c iworld
					= processControls` cs iworld
	
	//Extracts controls and resets the toplevel task list
	getControls iworld=:{parallelControls}
		= case 'Map'.get (toString GlobalTaskList) parallelControls of
			Just (_,controls)	= (controls, {iworld & parallelControls = 'Map'.put (toString GlobalTaskList)(0,[]) parallelControls})
			_					= ([],iworld)
	
	execControls [] queue iworld = (queue,iworld)
	execControls [c:cs] queue iworld=:{currentUser} = case c of
		AppendTask pid (container :: TaskContainer Void)
			//Make thread and properties
			# (thread,managerProperties) = case container of
				(DetachedTask props,tfun)	= (createThread (tfun GlobalTaskList), props)
				(WindowTask _,tfun)			= (createThread (tfun GlobalTaskList),{initManagerProperties & worker = currentUser})
				(DialogTask _,tfun)			= (createThread (tfun GlobalTaskList),{initManagerProperties & worker = currentUser})
				(BodyTask,tfun)				= (createThread (tfun GlobalTaskList),{initManagerProperties & worker = currentUser})
				(HiddenTask,tfun)			= (createThread (tfun GlobalTaskList),{initManagerProperties & worker = currentUser})
			//Make context
			# (context,iworld) = createContext (WorkflowProcess pid) thread managerProperties iworld			
			= execControls cs (queue ++ [context]) iworld
		//TODO: RemoveTask on the global list
		_
			= execControls cs queue iworld		
	
	
	topTarget (TaskContext _ {ProcessProperties|systemProperties=p=:{SystemProperties|taskId}}_ _)
		= taskNrFromString taskId
