implementation module TSt

import StdEnv, StdMaybe
import HSt, Util
import iDataFormlib
import ProcessDB, DynamicDB, SessionDB, TaskTree

//TODO: Create a better dynamic_string module with option for evaluating
//      a graph as far as possible to save a closure that is as small as
//		possible, but is still able to hold infinite structures
//

import code from "copy_graph_to_string.obj";
import code from "copy_graph_to_string_interface.obj";

mkTSt :: !Lifespan !Lifespan !Session ![Workflow]!*HSt -> *TSt
mkTSt itaskstorage threadstorage session workflows hst
	=	{ taskNr		= [-1]
		, userId		= -1
		, delegatorId	= -1
		, tree			= TTMainTask initTaskInfo initTaskProperties []
		, activated 	= True
		, newProcesses	= []
		, options 		= initialOptions itaskstorage
		, staticInfo	= initStaticInfo session threadstorage workflows
		, exceptions	= []
		, changeRequests= []
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
		, combination	= Nothing
		, trace			= False 
		}

initTaskInfo :: TaskInfo
initTaskInfo
	=	{ TaskInfo
		| taskId = ""
		, taskLabel = ""
		, active = True
		, finished = False
		, traceValue = ""
		}

initTaskProperties :: TaskProperties
initTaskProperties
	=	{ TaskProperties
		| processId	= 0
		, subject = ""
		, user = (-1, "")
		, delegator = (-1,"")
		, priority = NormalPriority
		, issuedAt = Time 0
		, firstEvent = Nothing
		, latestEvent = Nothing
		}

resetTSt :: *TSt -> *TSt
resetTSt tst = {tst & taskNr = [-1], activated = True, userId = -1}

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
	# (processes,tst)		= accHStTSt (getProcessesForUser currentUser [Active] True) tst	//Lookup all active processes for this user
	# (trees,tst)			= calculateTrees (sortProcesses processes) tst
	# (trees,tst)			= addNewProcesses (reverse trees) tst
	= (Nothing, trees, tst)	
where
	sortProcesses :: ![Process] -> [Process]
	sortProcesses ps = sortBy (\p1 p2 -> p1.Process.processId > p2.Process.processId) ps 

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

calculateTaskTree	:: !ProcessId !Bool !*TSt -> (!Maybe String, !Maybe TaskTree, !*TSt)
calculateTaskTree pid enableDebug tst
	# (mbProcess,tst)		= accHStTSt (getProcess pid) tst
	= case mbProcess of
		Just entry
			= case entry.Process.status of
				Active
					# (tree,tst)	= buildProcessTree entry tst
					= (Nothing, Just tree, tst)
				_
					= (Nothing, Just (initProcessNode entry), tst)
		Nothing
			= (Just "Process not found", Nothing, tst)
			
initProcessNode :: Process -> TaskTree
initProcessNode {processId, properties} = TTMainTask {TaskInfo|taskId = toString processId, taskLabel = properties.subject, active = True, finished = False, traceValue = "Process"} properties []

buildProcessTree :: Process !*TSt -> (!TaskTree, !*TSt)
buildProcessTree p =: {Process | processId, processType, status, properties = {TaskProperties|user,delegator}} tst
	# tst								= resetTSt tst
	# tst								= setTaskNr [-1,processId] tst
	# tst								= setUserId (fst user) tst
	# tst								= setDelegatorId (fst delegator) tst
	# tst								= setProcessId processId tst
	# tst								= setTaskTree (initProcessNode p) tst	
	# (result,tst)						= applyMainTask processType tst
	# (TTMainTask ti mti tasks, tst)	= getTaskTree tst
	# (finished, tst)					= taskFinished tst
	# (_,tst)							= accHStTSt (updateProcess processId (\p -> {Process
																					| p 
																					& status = if finished Finished Active
																					, result = result })) tst
	# tst								= garbageCollect finished processId tst
	= (TTMainTask {TaskInfo| ti & finished = finished} mti (reverse tasks), tst)
where
	applyMainTask (StaticProcess workflow) tst //Execute a static process
		# (mbWorkflow,tst)	= getWorkflowByName workflow tst
		= case mbWorkflow of
			Nothing
				= (Nothing, tst)
			Just {mainTask}
				# tst	= appTaskTSt mainTask tst
				= (Nothing, tst)
				
	applyMainTask (DynamicProcess task) tst //Execute a dynamic process
		# (mbTask, tst)		= accHStTSt (getDynamic task) tst
		= case mbTask of
			(Just dyn)
				# (result, tst)		= applyDynamicTask dyn tst
				#  result			= evalDynamicResult result
				# (finished, tst)	= taskFinished tst
				| finished
					# (resid, tst)	= accHStTSt (createDynamic (evalDynamicResult result)) tst
					= (Just resid, tst)
				| otherwise
					= (Nothing, tst)
			Nothing
				= (Nothing, tst)
				
	applyMainTask (EmbeddedProcess procid taskid) tst = (Nothing, tst) //Do nothing with embedded processes
	
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

addNewProcess :: !ProcessId !*TSt -> *TSt
addNewProcess pid tst = {tst & newProcesses = [pid:tst.newProcesses]}

getNewProcesses :: !*TSt -> (![ProcessId], !*TSt)
getNewProcesses tst =:{newProcesses} = (newProcesses, tst)

clearNewProcesses :: !*TSt -> *TSt
clearNewProcesses tst = {tst & newProcesses = []}
		
getEditorStates :: !String !*TSt	-> (![HtmlState], !*TSt)
getEditorStates prefix tst
	= accHStTSt (accFormStatesHSt (getHtmlStates prefix)) tst

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

import StdDebug, Text

mkBasicTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkBasicTask taskname taskfun = mkTask taskname taskfun nodefun id finfun
where
	nodefun info tst	= (TTBasicTask info [] [] [], tst)
	finfun tst=:{taskNr,tree=TTBasicTask info html inputs _}
		# (states, tst)	= accHStTSt (accFormStatesHSt (getHtmlStates (iTaskId taskNr "" +++ "-" ))) tst
		= {tst & tree = TTBasicTask info html inputs states}
	
mkSequenceTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkSequenceTask taskname taskfun = mkTask taskname taskfun nodefun nrfun id
where
	nodefun info tst	= (TTSequenceTask info [], tst)
	nrfun tst=:{taskNr}	= {tst & taskNr = [-1:taskNr]}
			
mkParallelTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkParallelTask taskname taskfun = mkTask taskname taskfun nodefun nrfun id
where
	nodefun info tst=:{TSt|options}	= case options.combination of
		Just combination	= (TTParallelTask info combination [], {TSt|tst & options = {options & combination = Nothing}})	//Apply combination and reset
		Nothing				= (TTParallelTask info TTVertical [], tst)	//Use default combination
			
	nrfun tst=:{taskNr}		= {tst & taskNr = [-1:taskNr]}
	
mkMainTask :: !String !TaskProperties !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkMainTask taskname mti taskfun = mkTask taskname taskfun nodefun nrfun id
where
	nodefun ti tst=:{taskNr, staticInfo={currentProcessId}}
		# taskId			= taskNrToString taskNr
		# (mbProps,tst)		= accHStTSt (getSubProcess currentProcessId taskId) tst //Lookup task properties in process table
		= case mbProps of
			(Just props)	= (TTMainTask ti props.Process.properties [], tst)
			Nothing
				# (_,tst)	= accHStTSt (createProcess (mkEmbeddedProcessEntry currentProcessId taskId mti Active currentProcessId)) tst //TODO use correct main task as direct parent 
				= (TTMainTask ti mti [], tst)
	nrfun tst=:{taskNr}		= {tst & taskNr = [-1:taskNr]}

mkTask :: !String !(*TSt -> *(!a,!*TSt)) !(TaskInfo *TSt -> *(TaskTree,*TSt)) (*TSt -> *TSt) (*TSt -> *TSt) -> Task a | iData a
mkTask taskname taskfun nodefun nrfun finfun = Task mkTask`
where
	mkTask` tst
		# (a, done, info, tst =:{taskNr,userId,delegatorId,tree,options}) = initTask taskname tst
		# (node, tst) = nodefun info {tst & taskNr = taskNr, userId = userId, delegatorId = delegatorId, tree = tree, options = options}
		| done
			= (a, {addTaskNode node tst & activated = True})
		| otherwise
			# (a, tst)		= executeTask (Task taskfun) (nrfun {tst & tree = node})
			# tst			= finfun tst
			# tst = addTaskNode
						(updateTaskNode tst.activated (printToString a) tst.tree)
						{tst & taskNr = taskNr, userId = userId, delegatorId = delegatorId, tree = tree, options = options}
			# tst = finalizeTask a tst
			= (a, tst)

	//Shared initial task creation part
	//- Increases the task number
	//- Checks if the task is done, and gets its value
	//- Creates a basicTask node when the task is done
	//initTask :: String *TSt -> *(a,Bool,TaskInfo,*TSt) | iData a
	initTask taskName tst
		# tst =:{taskNr,userId,delegatorId,hst,options,activated}
			= incTStTaskNr tst
		# (store,hst) 		= mkStoreForm (Init,storageFormId options (iTaskId taskNr "") (False,createDefault)) id hst
		# (finished,value)	= store.Form.value
		# info =	{ taskId		= taskNrToString taskNr
					, taskLabel		= taskName
					, active		= activated
					, finished		= finished
				 	, traceValue	= printToString value
				 	}
		
		= (value,finished,info,{tst & hst = hst})
	
	//Shared final task creation part
	//finalizeTask :: a *TSt -> *TSt | iData a
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
	//executeTask :: !(Task a) -> (*TSt -> (a,*TSt)) | iCreateAndPrint a
	executeTask task = executeTask`
	where
		executeTask` tst=:{activated}
			| activated
				= accTaskTSt task tst				// Perform task and get its result
			| otherwise								
				= (createDefault,tst)				// When a task is not active, don't execute it, return default value
	
	//update the finished and trace fields of a task tree node
	//updateTaskNode :: Bool String TaskTree -> TaskTree
	updateTaskNode finished traceValue (TTBasicTask ti output inputs states)	= TTBasicTask {ti & finished = finished, traceValue = traceValue} output inputs states
	updateTaskNode finished traceValue (TTSequenceTask ti tasks) 				= TTSequenceTask {ti & finished = finished, traceValue = traceValue} (reverse tasks)
	updateTaskNode finished traceValue (TTParallelTask ti combination tasks)	= TTParallelTask {ti & finished = finished, traceValue = traceValue} combination (reverse tasks)
	updateTaskNode finished traceValue (TTMainTask ti mti tasks)				= TTMainTask {ti & finished = finished, traceValue = traceValue} mti (reverse tasks)
		
	//Add a new node to the current sequence or process
	//addTaskNode :: TaskTree *TSt -> *TSt
	addTaskNode node tst=:{tree}
		= case tree of
			(TTMainTask ti mti tasks)				= {tst & tree = TTMainTask ti mti [node:tasks]}
			(TTSequenceTask ti tasks)				= {tst & tree = TTSequenceTask ti [node:tasks]}
			(TTParallelTask ti combination tasks)	= {tst & tree = TTParallelTask ti combination [node:tasks]}
			_										= {tst & tree = tree}

setOutput :: ![HtmlTag] !*TSt -> *TSt
setOutput output tst=:{tree}
	= case tree of
		(TTBasicTask info _ inputs states)			= {tst & tree = TTBasicTask info output inputs states}
		(TTParallelTask info combination branches)	= {tst & tree = TTParallelTask info combination branches}
		_											= {tst & tree = tree}
		
setInputs :: ![InputDefinition] !*TSt -> *TSt
setInputs inputs tst=:{tree}
	= case tree of
		(TTBasicTask info output _ states)	= {tst & tree = TTBasicTask info output inputs states}
		_									= {tst & tree = tree}

setCombination :: !TaskCombination !*TSt	-> *TSt
setCombination combination tst=:{tree}
	= case tree of 
		(TTParallelTask info _ branches)	= {tst & tree = TTParallelTask info combination branches}
		_									= {tst & tree = tree}

setNextCombination	:: !TaskCombination !*TSt	-> *TSt
setNextCombination newCombination tst=:{TSt|options = options=:{combination}}
	= case combination of
		Nothing	= {TSt|tst & options = {options & combination = Just newCombination}}
		_		= {TSt|tst & options = {options & combination = combination}}

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