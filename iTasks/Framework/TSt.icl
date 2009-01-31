implementation module TSt

import InternaliTasksCommon
import StdEnv, StdMaybe
import HSt
import ProcessDB, TaskTree

//TODO: Create a better dynamic_string module with option for evaluating
//      a graph as far as possible to save a closure that is as small as
//		possible, but is still able to hold infinite structures
//
import dynamic_string
import code from "copy_graph_to_string.obj";
import code from "copy_graph_to_string_interface.obj";

mkTSt :: !Lifespan !Lifespan !Session ![Workflow]!*HSt !*ProcessDB -> *TSt
mkTSt itaskstorage threadstorage session workflows hst processdb
	=	{ taskNr		= [-1]
		, userId		= -1
		, html 			= BT [] []
		, tree			= TTProcess {processId = -1, processLabel = "", userId = -1, finished = False} []
		, activated 	= True
		, users			= []
		, newProcesses	= []
		, options 		= initialOptions itaskstorage
		, staticInfo	= initStaticInfo session threadstorage workflows
		, hst 			= hst
		, processdb		= processdb
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
resetTSt tst = {tst & taskNr = [-1], activated = True, userId = -1, html = BT [] [], users = []}

setTaskNr :: TaskNr *TSt -> *TSt
setTaskNr taskNr tst = {TSt | tst & taskNr = taskNr}

setUserId :: UserId *TSt -> *TSt
setUserId userId tst = {TSt | tst & userId = userId,  users = [userId:tst.TSt.users]}

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
	# (processes,tst)		= accProcessDBTSt (getProcessesForUser currentUser [Active]) tst	//Lookup all active processes for this user
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
		# (processes,tst)	= accProcessDBTSt (getProcessesById pids) tst	//Lookup the process entries
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
	# (mbProcess,tst)		= accProcessDBTSt (getProcessForUser currentUser pid) tst
	= case mbProcess of
		Just entry
			# (tree,tst)	= buildProcessTree entry tst
			= (Nothing, Just tree, tst)
		Nothing
			= (Just "Process not found", Nothing, tst)

buildProcessTree :: Process !*TSt -> (!TaskTree, !*TSt)
buildProcessTree {Process | id, owner, status, process} tst
	# tst								= resetTSt tst
	# tst								= setTaskNr [-1,id] tst
	# tst								= setUserId owner tst
	# tst								= setProcessId id tst
	# tst								= setTaskTree (TTProcess {processId = id, processLabel = "", userId = owner, finished = False} []) tst	
	# (label,result,tst)				= applyMainTask process tst
	# (TTProcess info sequence, tst)	= getTaskTree tst
	# (users, tst)						= getUsers tst
	# (finished, tst)					= taskFinished tst
	# (_,tst)							= accProcessDBTSt (updateProcess (if finished Finished Active) result (removeDup users) id) tst 
	= (TTProcess {ProcessInfo|info & processLabel = label, finished = finished} (reverse sequence), tst)
where
	applyMainTask (LEFT {workflow}) tst //Execute a static process
		# (mbWorkflow,tst)	= getWorkflowByName workflow tst
		= case mbWorkflow of
			Nothing
				= ("", Nothing, tst)
			Just {mainTask,label}
				# tst	= appTaskTSt mainTask tst
				= (label, Nothing, tst)
				
	applyMainTask (RIGHT {label,task}) tst //Execute a dynamic process
		# (result, tst)		= applyDynamicTask (string_to_dynamic {c \\ c <-: task}) tst
		#  result			= evalDynamicResult result
		# (finished, tst)	= taskFinished tst
		| finished
			= (label, Just (dynamic_to_string (evalDynamicResult result)), tst)
		| otherwise
			= (label, Nothing, tst)
	
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

getHtmlTree	:: !*TSt 	-> (!HtmlTree, !*TSt)
getHtmlTree tst =: {html}
	= (html, {tst & html = html})

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

appProcessDBTSt	:: !.(*ProcessDB -> *ProcessDB)			!*TSt -> *TSt
appProcessDBTSt f tst=:{processdb}
	= {tst & processdb=f processdb}
	
accProcessDBTSt	:: !.(*ProcessDB -> *(.a,*ProcessDB))	!*TSt -> (.a,!*TSt)
accProcessDBTSt f tst=:{processdb}
	# (a,processdb) = f processdb
	= (a,{tst & processdb = processdb})

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

mkBasicTask :: !String !(Task a) -> Task a | iCreateAndPrint a
mkBasicTask taskname task = Task mkBasicTask`
where
	mkBasicTask` tst		
		# tst =:{taskNr,userId,activated,tree}
				= incTStTaskNr tst																			//Increase the task number
		# node	= TTBasicTask (mkTaskInfo taskNr taskname userId activated) [] []							//Create the task node
		# (a, tst =:{activated = finished, tree = TTBasicTask info output inputs})							//Execute the with the new node as context
				= accTaskTSt (executeTask taskname task) {tst & tree = node}																										
		# tst	= addTaskNode (TTBasicTask {TaskInfo|info & finished = finished} output inputs) {tst & tree = tree}	//Add the node to current context
		= (a, tst)

mkSequenceTask :: !String !(Task a) -> Task a | iCreateAndPrint a
mkSequenceTask taskname task = Task mkSequenceTask`
where
	mkSequenceTask` tst
		# tst =:{taskNr,userId,activated,tree}
				= incTStTaskNr tst																			//Increase the task number
		# node	= TTSequenceTask (mkTaskInfo taskNr taskname userId activated) []							//Create the task node
		# (a, tst =:{activated = finished, tree = TTSequenceTask info sequence})							//Execute the with the new node as context
				= accTaskTSt (executeTask taskname task) {tst & taskNr = [-1:taskNr], tree = node}			//and a shifted task number
		# tst	= addTaskNode (TTSequenceTask {TaskInfo|info & finished = finished} (reverse sequence)) {tst & tree = tree} 	//Add the node to current context
		= (a, tst)
		
mkParallelTask :: !String !(Task a) -> Task a | iCreateAndPrint a
mkParallelTask taskname task = Task mkParallelTask`
where
	mkParallelTask` tst	
		# tst =:{taskNr,userId,activated,tree}
				= incTStTaskNr tst																			//Increase the task number
		# node	= TTParallelTask (mkTaskInfo taskNr taskname userId activated) TTVertical []				//Create the task node
		# (a, tst =:{activated = finished, tree = TTParallelTask info combination branches})			//Execute the with the new node as context
				= accTaskTSt (executeTask taskname task) {tst & tree = node}
		# tst 	= addTaskNode (TTParallelTask {TaskInfo|info & finished = finished} combination (reverse branches)) {tst & tree = tree} 
		= (a, tst)
		
mkParallelSubTask :: !String !Int (Task a) -> Task a  | iCreateAndPrint a	 								
mkParallelSubTask taskname i task = Task mkParallelSubTask`
where
	mkParallelSubTask` tst=:{activated,taskNr,userId,tree}
		# node		= TTSequenceTask (mkTaskInfo [i:taskNr] taskname userId activated) []					//Create the task node
		# (a, tst =:{activated = finished, tree = TTSequenceTask info sequence})
					= accTaskTSt (executeTask taskname task) {tst & taskNr = [-1,i:taskNr], tree = node}	// two shifts are needed
		# tst		= addTaskNode (TTSequenceTask {TaskInfo|info & finished = finished} (reverse sequence)) {tst & tree = tree, taskNr = taskNr}
		= (a, tst)

mkTaskInfo :: TaskNr String UserId Bool -> TaskInfo
mkTaskInfo tasknr label userid active
	= {taskId = (taskNrToString tasknr), taskLabel = label, userId = userid, active = active, finished = False, priority = NormalPriority} 

//Execute the task when active, else return a default value
executeTask :: !String !(Task a) -> (Task a) | iCreateAndPrint a
executeTask taskname task = Task executeTask`
where
	executeTask` tst=:{activated,taskNr,userId,options}
		| activated
			# (val,tst=:{activated,html})		= accTaskTSt task tst				// Perform task and get its result
			# tst	= {tst & taskNr = taskNr, options = options, userId = userId}	// Restore taskNr, userId and and options
			| options.trace
				# tst = {tst & html = TaskTrace { trTaskNr = taskNrToString taskNr	// Add a trace node
												, trTaskName = taskname
												, trActivated = activated
												, trUserId = userId
												, trValue = printToString val
												, trOptions = options } html}							
				= (val,tst)
			| otherwise
				= (val,tst)
		| otherwise								
			= (createDefault,tst)													// When a task is not active, don't execute it, return default value

//Add a new node to the current sequence or process
addTaskNode :: !TaskTree !*TSt -> *TSt
addTaskNode node tst=:{tree}
	= case tree of
		(TTProcess info tasks)					= {tst & tree = TTProcess info [node:tasks]}
		(TTSequenceTask info tasks)				= {tst & tree = TTSequenceTask info [node:tasks]}
		(TTParallelTask info combination tasks)	= {tst & tree = TTParallelTask info combination [node:tasks]}
		_										= {tst & tree = tree}

//Increate the tasknr of the task state
incTStTaskNr :: *TSt -> *TSt
incTStTaskNr tst = {tst & taskNr = incTaskNr tst.taskNr}

setOutput :: ![HtmlTag] !*TSt -> *TSt
setOutput output tst=:{tree}
	= case tree of
		(TTBasicTask info _ inputs)					= {tst & tree = TTBasicTask info output inputs}
		(TTParallelTask info combination branches)	= {tst & tree = TTParallelTask info combination branches}
		_											= {tst & tree = tree}
		
setInputs :: ![InputId] !*TSt -> *TSt
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
deleteAllSubTasks [tx:txs] tst=:{hst,userId} 
	# hst	= deleteIData  (iTaskId userId (tl tx) "") hst
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