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
		, tree			= TTProcess {processId = 0, userId = 0} []
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
calculateTaskForest :: !Bool !*TSt -> (!Maybe String, ![(HtmlTree,TaskTree)], !*TSt)
calculateTaskForest enableDebug tst
	# (currentUser,tst)		= getCurrentUser tst
	# (processes,tst)		= accProcessDBTSt (getProcessesForUser currentUser [Active]) tst	//Lookup all active processes for this user
	# (trees,tst)			= calculateTrees (sortProcesses processes) tst
	# (trees,tst)			= addNewProcesses (reverse trees) tst
	= (Nothing, trees, tst)	
where
	sortProcesses :: ![Process] -> [Process]
	sortProcesses ps = sortBy (\p1 p2 -> p1.Process.id > p2.Process.id) ps 

	addNewProcesses :: ![(HtmlTree,TaskTree)] *TSt -> (![(HtmlTree,TaskTree)],!*TSt)
	addNewProcesses trees tst
		# (pids,tst)		= getNewProcesses tst
		| isEmpty pids		= (trees,tst)									//Nothing to do...
		# (processes,tst)	= accProcessDBTSt (getProcessesById pids) tst	//Lookup the process entries
		# tst				= clearNewProcesses tst							//Reset the list of new processes
		# (ntrees,tst)		= calculateTrees (sortProcesses processes) tst	//Calculate the additional task trees
		= addNewProcesses (trees ++ reverse ntrees) tst						//Recursively check for more new processes	

	calculateTrees :: ![Process] !*TSt -> (![(HtmlTree,TaskTree)], !*TSt)
	calculateTrees [] tst = ([],tst)
	calculateTrees [p:ps] tst
		# (tree,tst)	= buildProcessTree p tst
		# (trees,tst)	= calculateTrees ps tst
		= ([tree:trees],tst)

calculateTaskTree	:: !Int !Bool !*TSt -> (!Maybe String, !Maybe (HtmlTree,TaskTree), !*TSt)
calculateTaskTree pid enableDebug tst
	# (currentUser,tst)		= getCurrentUser tst
	# (mbProcess,tst)		= accProcessDBTSt (getProcessForUser currentUser pid) tst
	= case mbProcess of
		Just entry
			# (tree,tst)	= buildProcessTree entry tst
			= (Nothing, Just tree, tst)
		Nothing
			= (Just "Process not found", Nothing, tst)

buildProcessTree :: Process !*TSt -> (!(HtmlTree,TaskTree), !*TSt)
buildProcessTree {Process | id, owner, status, process} tst
	# tst				= resetTSt tst
	# tst				= setTaskNr [-1,id] tst
	# tst				= setUserId owner tst
	# tst				= setProcessId id tst
	# tst				= setTaskTree (TTProcess {processId = id, userId = owner} []) tst	
	# (label,mbRes,tst)	= applyMainTask process tst
	# (htree, tst)		= getHtmlTree tst
	# (ttree, tst)		= getTaskTree tst
	# (users, tst)		= getUsers tst
	# (finished, tst)	= taskFinished tst
	# (_,tst)			= accProcessDBTSt (updateProcess (if finished Finished Active) mbRes (removeDup users) id) tst 
	= ((description id owner label @@: htree, ttree ), tst)
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
	
	description :: Int Int String -> TaskDescription
	description pid owner label =	{ delegatorId	= owner
									, taskWorkerId	= owner
									, taskNrId		= toString pid
									, processNr		= pid
			
									, workflowLabel	= label
									, taskLabel		= "Main task"
									, timeCreated	= Time 0			//Store in process table!
									, taskPriority	= NormalPriority	//Store in process table!
									, curStatus		= False				//Not finished yet
									}

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


// mkTask is an important wrapper function which should be wrapped around any task
// It takes care of
//		- deciding whether the task should be called (activated) or not
//		- adding of trace information
//		- generating task numbers in a systematic way
// It is very important that the numbering of the tasks is done systematically
// Every task should have a unique number
// Every sequential task should increase the task number
// If a task j is a subtask of task i, than it will get number i.j in reverse order

mkTaskNoInc :: !String !(Task a) -> (Task a) | iCreateAndPrint a
mkTaskNoInc taskname mytask = Task mkTaskNoInc`
where
	mkTaskNoInc` tst=:{activated,taskNr,userId,options}		
		| not activated								= (createDefault,tst)		// When a task is not active, don't execute it, return default value
		# (val,tst=:{activated,html})				= accTaskTSt mytask tst		// active, so perform task and get its result
		# tst	= {tst & taskNr = taskNr, options = options, userId = userId}
		| options.trace || taskname == ""	= (val,tst)							// no trace, just return value
		# tst = {tst & html = TaskTrace {trTaskNr = taskNrToString taskNr, trTaskName = taskname, trActivated = activated, trUserId = userId, trValue = printToString val, trOptions = options} html}
		= (val,tst) 

mkTask :: !String !(Task a) -> (Task a) | iCreateAndPrint a
mkTask taskname task = Task mkTask`
where
	mkTask` tst =:{activated,taskNr,userId,options}		
		# tst = incTStTaskNr tst																	//Increase the task number
		# tst = addTaskNode (TTBasicTask (mkTaskInfo (incTaskNr taskNr) taskname userId activated) [] []) tst	//Add a node to the task tree
		# tst = accTaskTSt (mkTaskNoInc taskname task) tst											//Execute the task (if active)
		= tst
			
	//Add a new node to the current sequence
	addTaskNode node tst=:{tree}
		= case tree of
			(TTProcess info tasks)		= {tst & tree = TTProcess info [node:tasks]}
			(TTSequenceTask info tasks)	= {tst & tree = TTSequenceTask info [node:tasks]}
			_							= {tst & tree = tree}

mkParallelTask :: !String !(Task a) -> (Task a) | iCreateAndPrint a
mkParallelTask taskname task = Task mkParallelTask`
where
	mkParallelTask` tst =:{activated,taskNr,userId,options}		
		# tst = incTStTaskNr tst																			//Increase the task number
		# tst = addTaskNode (TTParallelTask (mkTaskInfo (incTaskNr taskNr) taskname userId activated) TTVertical [] []) tst	//Add a node to the task tree
		# tst = accTaskTSt (mkTaskNoInc taskname task) tst													//Execute the task (if active)
		= tst
		
	//Add a new node to the current sequence
	addTaskNode node tst=:{tree}
		= case tree of
			(TTProcess info tasks)		= {tst & tree = TTProcess info [node:tasks]}
			(TTSequenceTask info tasks)	= {tst & tree = TTSequenceTask info [node:tasks]}
			_							= {tst & tree = tree}

mkParallelSubTask :: !String !Int (Task a) -> (Task a)  | iCreateAndPrint a	 // two shifts are needed
mkParallelSubTask name i task = Task mkParallelSubTask`
where
	mkParallelSubTask` tst=:{activated,taskNr,userId,options,tree}
		//Create a sub task name
		# taskname	= name +++ "." +++ toString i
		//Shift task number by two positions and start with a blank sequence node
		# tst		= {tst & taskNr = [-1,i:taskNr], tree = TTSequenceTask (mkTaskInfo [i:taskNr] taskname userId activated) [], activated = True}
		//Execute the parallel task
		# (a,tst=:{tree = sequence})
			= accTaskTSt (mkTaskNoInc taskname task) tst
		//Continue with the original tasknr, options and tasktree, but with the new sequence as subtree 
		= (a, {tst & taskNr = taskNr, options = options, tree = addSubTaskNode sequence tree})

	//Add the parallel executed sequence to the TTParallelTask node at the head of the
	//the tasks of the current process or sequence
	addSubTaskNode node (TTProcess info1 [TTParallelTask info2 combination output tasks2 :tasks1])
		= TTProcess info1 [TTParallelTask info2 combination output [node:tasks2] : tasks1]
	addSubTaskNode node (TTSequenceTask info1 [TTParallelTask info2 combination output tasks2 :tasks1])
		= TTSequenceTask info1 [TTParallelTask info2 combination output [node:tasks2] : tasks1]
	addSubTaskNode node tree = tree

mkTaskInfo :: TaskNr String UserId Bool -> TaskInfo
mkTaskInfo tasknr label userid active
	= {taskId = (taskNrToString tasknr), taskLabel = label, userId = userid, active = active, priority = NormalPriority} 

incTStTaskNr :: *TSt -> *TSt
incTStTaskNr tst = {tst & taskNr = incTaskNr tst.taskNr}

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