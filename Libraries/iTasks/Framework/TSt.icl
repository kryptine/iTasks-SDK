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
		, mainTask		= -1
		, newProcesses	= []
		, options 		= initialOptions itaskstorage
		, staticInfo	= initStaticInfo session threadstorage workflows
		, exception		= Nothing
		, doChange		= False
		, changes		= []
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
		, deadline = Nothing
		, progress = TPActive
		, issuedAt = Time 0
		, firstEvent = Nothing
		, latestEvent = Nothing
		}





setProcessId :: ProcessId *TSt -> *TSt
setProcessId processId tst=:{staticInfo} = {TSt | tst & staticInfo = {staticInfo & currentProcessId = processId}}


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
	# (processes,tst)		= getProcessesForUser currentUser [Active] True tst	//Lookup all active processes for this user
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
		# (processes,tst)	= getProcessesById pids tst						//Lookup the process entries
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
	# (mbProcess,tst)		= getProcess pid tst
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
initProcessNode {processId, properties}
		= TTMainTask {TaskInfo|taskId = toString processId, taskLabel = properties.subject, active = True, finished = False, traceValue = "Process"} properties []

buildProcessTree :: Process !*TSt -> (!TaskTree, !*TSt)
buildProcessTree p =: {Process | processId, processType, properties = {TaskProperties|user,delegator}, changes} tst =:{staticInfo}
	# tst								= {TSt|tst	& taskNr = [-1,processId], activated = True, userId = (fst user), delegatorId = (fst delegator)
													, staticInfo = {StaticInfo|staticInfo & currentProcessId = processId}, tree = initProcessNode p, mainTask = processId}
	# tst								= loadChanges changes tst	
	# (result,tst)						= applyMainTask processType tst
	# (TTMainTask ti mti tasks, tst)	= getTaskTree tst
	# (finished, tst)					= taskFinished tst
	| finished
		# tst							= deleteTaskStates [processId] tst														//Garbage collect
		# (_,tst)						= updateProcess processId (\p -> {Process|p & status = Finished, result = result }) tst	//Save result
		= (TTMainTask {TaskInfo| ti & finished = True} mti (reverse tasks), tst)
	| otherwise
		# tst							= storeChanges processId tst
		= (TTMainTask ti mti tasks, tst)
where
	loadChanges changes tst = loadChanges` changes [] tst
	loadChanges` [] accu tst
		= {TSt|tst& changes = reverse accu}
	loadChanges` [(l,c):cs] accu tst
		# (dyn,tst) = accHStTSt (getDynamic c) tst
		= case dyn of
			Just dyn	= loadChanges` cs [(l,c,dyn):accu] tst
			Nothing		= loadChanges` cs accu tst
	
	storeChanges pid tst=:{TSt|changes} = storeChanges` changes [] pid tst
	storeChanges` [] accu pid tst
		# (_,tst)	= updateProcess pid (\p -> {Process|p & changes = reverse accu}) tst
		= tst
	storeChanges` [(l,c,d):cs] accu pid tst
		| c == 0
			# (c,tst)	= createDynamic d tst
			= storeChanges` cs [(l,c):accu] pid tst
		| otherwise
			# (_,tst)	= updateDynamic d c tst
			= storeChanges` cs [(l,c):accu] pid tst
			
	applyMainTask (StaticProcess workflow) tst //Execute a static process
		# (mbWorkflow,tst)	= getWorkflowByName workflow tst
		= case mbWorkflow of
			Nothing
				= (Nothing, tst)
			Just {Workflow|mainTask}
				# (_,tst)	= applyTask mainTask tst
				= (Nothing, tst)				
	applyMainTask (DynamicProcess task) tst //Execute a dynamic process
		# (mbTask, tst)		= getDynamic task tst
		= case mbTask of
			(Just dyn)
				# (result, tst)		= applyDynamicTask dyn tst
				#  result			= evalDynamicResult result
				# (finished, tst)	= taskFinished tst
				| finished
					# (resid, tst)	= createDynamic (evalDynamicResult result) tst
					= (Just resid, tst)
				| otherwise
					= (Nothing, tst)
			Nothing
				= (Nothing, tst)			
	applyMainTask (EmbeddedProcess procid taskid) tst = (Nothing, tst) //Do nothing with embedded processes
	
	/**
	* This forces evaluation of the dynamic to normal form before we encode it
	*/
	evalDynamicResult :: !Dynamic -> Dynamic
	evalDynamicResult d = code {
		push_a 0
		.d 1 0
		jsr	_eval_to_nf
		.o 0 0
	}
	
	applyDynamicTask :: !Dynamic !*TSt -> (!Dynamic, !*TSt)
	applyDynamicTask (task :: (Task Dynamic)) tst = applyTask task tst 
	

applyChange :: !ProcessId !String !Dynamic !*TSt -> *TSt
applyChange pid name change tst
	# (mbProcess,tst)		= getProcess pid tst
	= case mbProcess of
		Just proc
			= snd (buildProcessTree proc {tst & doChange = True, changes = [(name,0,change)]})
		Nothing
			= tst

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
mkBasicTask taskname taskfun = mkTask taskname initfun finfun
where
	initfun info tst
		= (taskfun, {tst & tree = TTBasicTask info [] [] []})
	finfun tst=:{taskNr,tree=TTBasicTask info html inputs _}
		# (states, tst)	= accHStTSt (accFormStatesHSt (getHtmlStates (iTaskId taskNr "" +++ "-" ))) tst
		= {tst & tree = TTBasicTask info html inputs states}
	
mkSequenceTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkSequenceTask taskname taskfun = mkTask taskname initfun id
where
	initfun info tst=:{TSt|taskNr}	= (taskfun,{tst & tree = TTSequenceTask info [], taskNr = [-1:taskNr]})
			
mkParallelTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkParallelTask taskname taskfun = mkTask taskname initfun id
where
	initfun info tst=:{TSt|taskNr,options}	= case options.combination of
		//Apply combination and reset
		Just combination	= (taskfun,{tst & tree = TTParallelTask info combination [], taskNr = [-1:taskNr], options = {options & combination = Nothing}})
		//Use default combination
		Nothing				= (taskfun,{tst & tree = TTParallelTask info TTVertical [], taskNr = [-1:taskNr]})												
		
mkMainTask :: !String !TaskProperties !(Task a) -> Task a | iData a
mkMainTask taskname initProperties task = mkTask taskname prefun id
where
	prefun taskInfo tst=:{taskNr, mainTask, staticInfo={currentProcessId}}
		# taskId						= taskNrToString taskNr
		# (mbProc,tst)					= getSubProcess currentProcessId taskId tst 
		# (properties, processId, curfun, curfunid, changeNr, tst=:{doChange,changes})
			= case mbProc of
				(Just {Process | properties, processId, taskfun, changeNr})
					| isNothing taskfun
						= (properties, processId, applyTask task, 0, changeNr, tst)
					| otherwise
						# fid			= fromJust taskfun
						# (mbFun,tst)	= getDynamic fid tst
						= case mbFun of
							(Just (f :: *TSt -> *(a^,*TSt)))	= (properties, processId, f, fid, changeNr, tst)
							_									= (properties, processId, applyTask task, 0, changeNr, tst)
				Nothing
					# (processId, tst)	= createProcess (mkEmbeddedProcessEntry currentProcessId taskId initProperties Active mainTask) tst
					= (initProperties, processId, applyTask task, 0, 0, tst)
		| doChange
			= case changes of
				[(label,cid,(Change changefun :: Change a^)):rest]	= do_change processId taskInfo properties changeNr curfun curfunid (label,cid,changefun) rest tst
				other												= no_change processId taskInfo properties changeNr curfun tst
		| otherwise
			= no_change processId taskInfo properties changeNr curfun tst
		where
			do_change processId taskInfo properties changeNr curfun curfunid (label,cid,changefun) rest tst
			 	# (mbProperties, mbTask, mbChange) = changefun properties (Task (Just [-1,changeNr:taskNr]) curfun) task
				//Determine new change list
				# changes = case mbChange of
						(Just change)	= [(label,cid,dynamic change):rest]
						_				= rest
				//Update task (and properties when changed) 	
				| isJust mbTask
					# changeNr			= inc changeNr 
					# properties		= if (isJust mbProperties) (fromJust mbProperties) properties
					# curfun			= applyTask (fromJust mbTask)					
					# (curfunid,tst)	= updateTaskfunDynamic curfunid (dynamic curfun) tst 
					# (_,tst) 			= updateProcess processId (\p -> {p & taskfun = Just curfunid, properties = properties, changeNr = changeNr }) tst 
					= (curfun, {tst & tree = TTMainTask taskInfo properties [], taskNr = [-1,changeNr:taskNr], mainTask = processId, changes = changes})
				//Only add properties
				| isJust mbProperties
					# properties		= fromJust mbProperties
					# (_,tst) 			= updateProcess processId (\p -> {p & properties = properties}) tst
					= (curfun, {tst & tree = TTMainTask taskInfo properties [], taskNr = [-1,changeNr:taskNr], mainTask = processId, changes = changes})
				// Task and properties unchanged
				| otherwise
					= no_change processId taskInfo properties changeNr curfun {TSt|tst & changes = changes}
			where
				updateTaskfunDynamic 0 d tst
					= createDynamic d tst
				updateTaskfunDynamic i d tst
					# (_,tst) = updateDynamic d i tst
					= (i,tst)
				
 			no_change processId taskInfo properties changeNr curfun tst
 				= (curfun, {tst & tree = TTMainTask taskInfo properties [], taskNr = [-1,changeNr:taskNr], mainTask = processId})

mkTask :: !String !(TaskInfo *TSt -> (*TSt -> *(!a,!*TSt),*TSt)) (*TSt -> *TSt) -> Task a | iData a
mkTask taskname prefun postfun = Task Nothing mkTask`
where
	mkTask` tst
		# (a, done, info, tst =:{taskNr,userId,delegatorId,tree,options}) = initTask taskname tst
		| done
			= (a, {addTaskNode (TTBasicTask info [] [] []) tst & activated = True})
		| otherwise
			# (taskfun, tst)	= prefun info {tst & taskNr = taskNr, userId = userId, delegatorId = delegatorId, tree = tree, options = options}
			# (a, tst)			= executeTask taskfun tst
			# tst				= postfun tst
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
					, active		= activated
					, finished		= finished
				 	, traceValue	= printToString value
				 	}
		
		= (value,finished,info,{tst & hst = hst})
	
	//Shared final task creation part
	finalizeTask value tst =:{taskNr,activated,options}
		| activated
			# tst=:{hst}		= deleteTaskStates taskNr tst //Garbage collect
			# (store,hst) 		= mkStoreForm (Init,storageFormId options (iTaskId taskNr "") (False,createDefault)) (\_ -> (True,value)) hst
			= {tst & hst = hst}
		| otherwise
			= tst

	//Increas the task nr
	incTaskNr [] = [0]
	incTaskNr [i:is] = [i+1:is]
		
	//Increate the tasknr of the task state
	incTStTaskNr tst = {tst & taskNr = incTaskNr tst.taskNr}
	
	//Execute the task when active, else return a default value
	executeTask taskfun = executeTask`
	where
		executeTask` tst=:{activated}
			| activated
				= taskfun tst				// Perform task and get its result
			| otherwise								
				= (createDefault,tst)		// When a task is not active, don't execute it, return default value
	
	//update the finished and trace fields of a task tree node
	updateTaskNode finished traceValue (TTBasicTask ti output inputs states)	= TTBasicTask {ti & finished = finished, traceValue = traceValue} output inputs states
	updateTaskNode finished traceValue (TTSequenceTask ti tasks) 				= TTSequenceTask {ti & finished = finished, traceValue = traceValue} (reverse tasks)
	updateTaskNode finished traceValue (TTParallelTask ti combination tasks)	= TTParallelTask {ti & finished = finished, traceValue = traceValue} combination (reverse tasks)
	updateTaskNode finished traceValue (TTMainTask ti mti tasks)				= TTMainTask {ti & finished = finished, traceValue = traceValue} mti (reverse tasks)
		
	//Add a new node to the current sequence or process
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

deleteTaskStates :: !TaskNr !*TSt -> *TSt
deleteTaskStates tasknr tst = appHStTSt (deleteIData (iTaskId tasknr "")) tst

copyTaskStates :: !TaskNr !TaskNr !*TSt	-> *TSt
copyTaskStates fromtask totask tst = appHStTSt (copyIData (iTaskId fromtask "") (iTaskId totask "")) tst

//TODO: Migrate existing state
applyTask :: !(Task a) !*TSt -> (!a,!*TSt)
applyTask (Task _ fn) tst = fn tst
			
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