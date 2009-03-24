implementation module TSt

import StdEnv, StdMaybe
import HSt, Util
import iDataFormlib
import ProcessDB, TaskTree

//TODO: Create a better dynamic_string module with option for evaluating
//      a graph as far as possible to save a closure that is as small as
//		possible, but is still able to hold infinite structures
//
import dynamic_string
import code from "copy_graph_to_string.obj";
import code from "copy_graph_to_string_interface.obj";

mkTSt :: !Lifespan !Lifespan !Session ![Workflow]!*HSt -> *TSt
mkTSt itaskstorage threadstorage session workflows hst
	=	{ taskNr		= [-1]
		, userId		= -1
		, delegatorId	= -1
		, tree			= TTProcess {processId = -1, processLabel = "", userId = -1, delegatorId = -1, status = Active} []
		, activated 	= True
		, users			= []
		, newProcesses	= []
		, options 		= initialOptions itaskstorage
		, staticInfo	= initStaticInfo session threadstorage workflows
		, exceptions	= []
		, hst 			= hst
		}

initStaticInfo :: !Session !Lifespan ![Workflow] -> StaticInfo
initStaticInfo session location workflows
	=	{ currentUserId		= session.Session.userId
		, currentProcessId	= -1
		, currentSession 	= session
		, threadTableLoc	= location
		, staticWorkflows	= workflows
		}

initialOptions :: !Lifespan -> Options 
initialOptions location 
	=	{ tasklife 		= location 
		, taskstorage 	= PlainString
		, taskmode 		= Edit
		, trace			= False 
		, gc			= Collect
		}

resetTSt :: *TSt -> *TSt
resetTSt tst = {tst & taskNr = [-1], activated = True, userId = -1, users = []}

setTaskNr :: TaskNr *TSt -> *TSt
setTaskNr taskNr tst = {TSt | tst & taskNr = taskNr}

setUserId :: UserId *TSt -> *TSt
setUserId userId tst = {TSt | tst & userId = userId}

setDelegatorId :: UserId *TSt -> *TSt
setDelegatorId delegatorId tst = {TSt | tst & delegatorId = delegatorId}

setProcessId :: ProcessId *TSt -> *TSt
setProcessId processId tst=:{staticInfo} = {TSt | tst & staticInfo = {staticInfo & currentProcessId = processId}}

setTaskTree :: !TaskTree !*TSt	-> *TSt
setTaskTree tree tst = {tst & tree = tree}


/*
* When the complete task forest for a certain user is calculated, we do this
* in a specific order to make sure that we get the complete forest and that
* sub processes are always calculated before their parent processes because since
* these parent processes may be waiting for their final value.
*
* The informal algorithm to calculate the forest is as follows:
* - Retrieve all processes for the current user from the database
* - Sort these processes on process id in descending order
*  (this ensures that child processes are executed before their parent)
* - Calculate the tasktrees for the sorted list of processes
* - Reverse the list to get the task trees in ascending order
* - Check the TSt for newly created process ids for the current user
* - When there are new processes calculate their tasktrees and append
*   them to the list.
* - Check if the new processes have spawned new processes themselves
* - repeat...
*/
calculateTaskForest :: !Bool !*TSt -> (!Maybe String, ![TaskTree], !*TSt)
calculateTaskForest enableDebug tst
	# (currentUser,tst)		= getCurrentUser tst
	# (processes,tst)		= accHStTSt (getProcessesForUser currentUser [Active]) tst	//Lookup all active processes for this user
	# (trees,tst)			= calculateTrees (sortProcesses processes) tst
	# (trees,tst)			= addNewProcesses (reverse trees) tst
	= (Nothing, trees, tst)	
where
	sortProcesses :: ![Process] -> [Process]
	sortProcesses ps = sortBy (\p1 p2 -> p1.Process.id > p2.Process.id) ps 

	addNewProcesses :: ![TaskTree] *TSt -> (![TaskTree],!*TSt)
	addNewProcesses trees tst
		# (pids,tst)		= getNewProcesses tst
		| isEmpty pids		= (trees,tst)									//Nothing to do...
		# (processes,tst)	= accHStTSt (getProcessesById pids) tst			//Lookup the process entries
		# tst				= clearNewProcesses tst							//Reset the list of new processes
		# (ntrees,tst)		= calculateTrees (sortProcesses processes) tst	//Calculate the additional task trees
		= addNewProcesses (trees ++ reverse ntrees) tst						//Recursively check for more new processes	

	calculateTrees :: ![Process] !*TSt -> (![TaskTree], !*TSt)
	calculateTrees [] tst = ([],tst)
	calculateTrees [p:ps] tst
		# (tree,tst)	= buildProcessTree p tst
		# (trees,tst)	= calculateTrees ps tst
		= ([tree:trees],tst)

calculateTaskTree	:: !Int !Bool !*TSt -> (!Maybe String, !Maybe TaskTree, !*TSt)
calculateTaskTree pid enableDebug tst
	# (currentUser,tst)		= getCurrentUser tst
	# (mbProcess,tst)		= accHStTSt (getProcessForUser currentUser pid) tst
	= case mbProcess of
		Just entry
			= case entry.Process.status of
				Active
					# (tree,tst)	= buildProcessTree entry tst
					= (Nothing, Just tree, tst)
				_
					= (Nothing, Just (TTProcess {processId = entry.Process.id, processLabel = "", userId = entry.owner, delegatorId = entry.delegator, status = entry.Process.status} []), tst)
		Nothing
			= (Just "Process not found", Nothing, tst)

buildProcessTree :: Process !*TSt -> (!TaskTree, !*TSt)
buildProcessTree {Process | id, label, owner, delegator, status, process} tst
	# tst								= resetTSt tst
	# tst								= setTaskNr [-1,id] tst
	# tst								= setUserId owner tst
	# tst								= setDelegatorId delegator tst
	# tst								= setProcessId id tst
	# tst								= setTaskTree (TTProcess {processId = id, processLabel = label, userId = owner, delegatorId = delegator, status = status} []) tst	
	# (result,tst)						= applyMainTask process tst
	# (TTProcess info sequence, tst)	= getTaskTree tst
	# (users, tst)						= getUsers tst
	# (finished, tst)					= taskFinished tst
	# (_,tst)							= accHStTSt (updateProcess (if finished Finished Active) result (removeDup users) id) tst
	# tst								= garbageCollect finished id tst
	= (TTProcess {ProcessInfo | info & status = (if finished Finished Active)} (reverse sequence), tst)
where
	applyMainTask (LEFT {workflow}) tst //Execute a static process
		# (mbWorkflow,tst)	= getWorkflowByName workflow tst
		= case mbWorkflow of
			Nothing
				= (Nothing, tst)
			Just {mainTask}
				# tst	= appTaskTSt mainTask tst
				= (Nothing, tst)
				
	applyMainTask (RIGHT {task}) tst //Execute a dynamic process
		# (result, tst)		= applyDynamicTask (string_to_dynamic {c \\ c <-: task}) tst
		#  result			= evalDynamicResult result
		# (finished, tst)	= taskFinished tst
		| finished
			= (Just (dynamic_to_string (evalDynamicResult result)), tst)
		| otherwise
			= (Nothing, tst)
	
	
	garbageCollect False id tst	= tst
	garbageCollect True id tst	= appHStTSt (deleteIData (iTaskId [id] "")) tst
	
	/**
	* This evaluates the dynamic to normal form before we encode it
	*/
	evalDynamicResult :: !Dynamic -> Dynamic
	evalDynamicResult d = code {
		push_a 0
		.d 1 0
		jsr	_eval_to_nf
		.o 0 0
	}
	
	applyDynamicTask :: !Dynamic !*TSt -> (!Dynamic, !*TSt)
	applyDynamicTask (dyntask :: (Task Dynamic)) tst = accTaskTSt dyntask tst 
	

getCurrentSession :: !*TSt 	-> (!Session, !*TSt)
getCurrentSession tst =:{staticInfo} = (staticInfo.currentSession, tst)

getCurrentUser :: !*TSt -> (!UserId, !*TSt)
getCurrentUser tst =: {staticInfo}
	= (staticInfo.currentUserId, {tst & staticInfo = staticInfo})

getCurrentProcess :: !*TSt -> (!ProcessId, !*TSt)
getCurrentProcess tst =: {staticInfo}
	= (staticInfo.currentProcessId, {tst & staticInfo = staticInfo})

getTaskTree :: !*TSt	-> (!TaskTree, !*TSt)
getTaskTree tst =: {tree}
	= (tree, {tst & tree = tree})

getWorkflows :: !*TSt -> (![Workflow],!*TSt)
getWorkflows tst=:{staticInfo = staticInfo =:{staticWorkflows}}
	= (staticWorkflows, {tst & staticInfo = {staticInfo & staticWorkflows = staticWorkflows}})

getWorkflowByName :: !String !*TSt -> (!Maybe Workflow, !*TSt)
getWorkflowByName name tst
	# (workflows, tst)	= getWorkflows tst
	= case filter (\wf -> wf.Workflow.name == name) workflows of
		[workflow]	= (Just workflow, tst)
		_			= (Nothing,tst)

addUser :: !UserId !*TSt -> *TSt
addUser uid tst = {TSt | tst & users = [uid:tst.TSt.users]}

getUsers :: !*TSt -> (![UserId],!*TSt)
getUsers tst=:{TSt|users} = (users,tst) 

addNewProcess :: !ProcessId !*TSt -> *TSt
addNewProcess pid tst = {tst & newProcesses = [pid:tst.newProcesses]}

getNewProcesses :: !*TSt -> (![ProcessId], !*TSt)
getNewProcesses tst =:{newProcesses} = (newProcesses, tst)

clearNewProcesses :: !*TSt -> *TSt
clearNewProcesses tst = {tst & newProcesses = []}
		
getEditorStates :: !*TSt	-> (![HtmlState], !*TSt)
getEditorStates tst
	= accHStTSt getPageStates tst

taskFinished :: !*TSt -> (!Bool, !*TSt)
taskFinished tst=:{activated} = (activated, {tst & activated = activated})

appHStTSt :: !.(*HSt -> *HSt) !*TSt -> *TSt
appHStTSt f tst=:{hst}
	= {tst & hst=f hst}

accHStTSt :: !.(*HSt -> *(.a,*HSt)) !*TSt -> (.a,!*TSt)
accHStTSt f tst=:{hst}
	# (a,hst) = f hst
	= (a,{tst & hst = hst})

appTaskTSt :: !(Task a) !*TSt -> *TSt
appTaskTSt (Task fn) tst = snd (fn tst)

accTaskTSt :: !(Task a) !*TSt -> (!a,!*TSt)
accTaskTSt (Task fn) tst = fn tst

// mkBasicTask is an important wrapper function which should be wrapped around any task
// It takes care of
//		- deciding whether the task should be called (activated) or not
//		- adding of trace information
//		- generating task numbers in a systematic way
// It is very important that the numbering of the tasks is done systematically
// Every task should have a unique number
// Every sequential task should increase the task number
// If a task j is a subtask of task i, than it will get number i.j in reverse order

mkBasicTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkBasicTask taskname taskfun = mkTask taskname taskfun nodefun id
where
	nodefun info	= TTBasicTask info [] []

mkSequenceTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkSequenceTask taskname taskfun = mkTask taskname taskfun nodefun tstfun
where
	nodefun info			= TTSequenceTask info []
	tstfun tst=:{taskNr}	= {tst & taskNr = [-1:taskNr]}
			
mkParallelTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkParallelTask taskname taskfun = mkTask taskname taskfun nodefun tstfun
where
	nodefun info			= TTParallelTask info TTVertical []
	tstfun tst=:{taskNr}	= {tst & taskNr = [-1:taskNr]}

mkTask :: !String !(*TSt -> *(!a,!*TSt)) (TaskInfo -> TaskTree) (*TSt -> *TSt) -> Task a | iData a
mkTask taskname taskfun nodefun tstfun = Task mkTask`
where
	mkTask` tst
		# (a,done,info,tst=:{taskNr,userId,delegatorId,tree,options}) = initTask taskname tst
		| done
			# tst = addTaskNode (nodefun info) tst
			= (a, {tst & activated = True})
		| otherwise
			# (a, tst)	
				= executeTask (Task taskfun) (tstfun {tst & tree = (nodefun info)})
			# tst = addTaskNode 
						(updateTaskNode tst.activated (printToString a) tst.tree)
						{tst & taskNr = taskNr, userId = userId, delegatorId = delegatorId, tree = tree, options = options}
			# tst = finalizeTask a tst
			= (a, tst)

	//Shared initial task creation part
	//- Increases the task number
	//- Checks if the task is done, and gets its value
	//- Creates a basicTask node when the task is done
	initTask taskName tst
		# tst =:{taskNr,userId,delegatorId,hst,options,activated}
			= incTStTaskNr tst
		# (store,hst) 		= mkStoreForm (Init,storageFormId options (iTaskId taskNr "") (False,createDefault)) id hst
		# (finished,value)	= store.Form.value
		# info =	{ taskId		= taskNrToString taskNr
					, taskLabel		= taskName
					, userId		= userId
					, delegatorId	= delegatorId
					, active		= activated
					, finished		= finished								
					, priority		= NormalPriority
				 	, traceValue	= printToString value
				 	}
		
		= (value,finished,info,{tst & hst = hst})
	
	//Shared final task creation part
	finalizeTask value tst =:{taskNr,activated,hst,options}
		| activated
			# hst				= deleteIData (iTaskId taskNr "") hst //Garbage collect
			# (store,hst) 		= mkStoreForm (Init,storageFormId options (iTaskId taskNr "") (False,createDefault)) (\_ -> (True,value)) hst
			= {tst & hst = hst}
		| otherwise
			= {tst & hst = hst}
	
	//Increate the tasknr of the task state
	//incTStTaskNr :: *TSt -> *TSt
	incTStTaskNr tst = {tst & taskNr = incTaskNr tst.taskNr}


	//Execute the task when active, else return a default value
	executeTask :: !(Task a) -> (*TSt -> (a,*TSt)) | iCreateAndPrint a
	executeTask task = executeTask`
	where
		executeTask` tst=:{activated}
			| activated
				= accTaskTSt task tst				// Perform task and get its result
			| otherwise								
				= (createDefault,tst)				// When a task is not active, don't execute it, return default value

	//update the finished and trace fields of a task tree node
	updateTaskNode finished traceValue (TTBasicTask info output inputs) = TTBasicTask {info & finished = finished, traceValue = traceValue} output inputs
	updateTaskNode finished traceValue (TTSequenceTask info tasks) = TTSequenceTask {info & finished = finished, traceValue = traceValue} (reverse tasks)
	updateTaskNode finished traceValue (TTParallelTask info combination tasks) = TTParallelTask {info & finished = finished, traceValue = traceValue} combination (reverse tasks)
	updateTaskNode finished traceValue (TTProcess info tasks) = TTProcess info tasks
	
	//Add a new node to the current sequence or process
	addTaskNode node tst=:{tree}
		= case tree of
			(TTProcess info tasks)					= {tst & tree = TTProcess info [node:tasks]}
			(TTSequenceTask info tasks)				= {tst & tree = TTSequenceTask info [node:tasks]}
			(TTParallelTask info combination tasks)	= {tst & tree = TTParallelTask info combination [node:tasks]}
			_										= {tst & tree = tree}


setOutput :: ![HtmlTag] !*TSt -> *TSt
setOutput output tst=:{tree}
	= case tree of
		(TTBasicTask info _ inputs)					= {tst & tree = TTBasicTask info output inputs}
		(TTParallelTask info combination branches)	= {tst & tree = TTParallelTask info combination branches}
		_											= {tst & tree = tree}
		
setInputs :: ![InputDefinition] !*TSt -> *TSt
setInputs inputs tst=:{tree}
	= case tree of
		(TTBasicTask info output _)	= {tst & tree = TTBasicTask info output inputs}
		_							= {tst & tree = tree}

setCombination :: !TaskCombination !*TSt	-> *TSt
setCombination combination tst=:{tree}
	= case tree of 
		(TTParallelTask info _ branches)	= {tst & tree = TTParallelTask info combination branches}
		_									= {tst & tree = tree}

resetSequence :: !*TSt -> *TSt
resetSequence tst=:{taskNr,tree}
	= case tree of
		(TTSequenceTask info sequence)	= {tst & taskNr = [-1:tl taskNr], tree = TTSequenceTask info []}
		_								= {tst & tree = tree}
				
deleteAllSubTasks :: ![TaskNr] TSt -> TSt
deleteAllSubTasks [] tst = tst
deleteAllSubTasks [tx:txs] tst=:{hst} 
	# hst	= deleteIData  (iTaskId (tl tx) "") hst
	= deleteAllSubTasks txs {tst & hst = hst}


incTaskNr :: !TaskNr -> TaskNr
incTaskNr [] = [0]
incTaskNr [i:is] = [i+1:is]

taskNrToString :: !TaskNr -> String
taskNrToString [] 		= ""
taskNrToString [i] 		= toString i
taskNrToString [i:is] 	= taskNrToString is +++ "." +++ toString i 

taskNrFromString :: !String -> TaskNr
taskNrFromString "" 		= []
taskNrFromString string	= reverse (parseTaskNr` [char \\ char <-: string])
where
	parseTaskNr` :: ![Char] -> TaskNr
	parseTaskNr` [] = []
	parseTaskNr` list 
	# (front,end)	= span (\c -> c <> '.') list
	=  [toInt (toString  front) : parseTaskNr` (stl end)]

	toString :: [Char] -> String
	toString list = {c \\ c <- list}

	stl :: [Char] -> [Char]
	stl [] = []
	stl xs = tl xs

taskNrToProcessNr :: !TaskNr -> ProcessNr
taskNrToProcessNr []	= -1
taskNrToProcessNr l 	= last l