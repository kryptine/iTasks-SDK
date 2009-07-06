implementation module TSt

import StdEnv, StdMaybe
import HSt, Util
import iDataFormlib
import ProcessDB, DynamicDB, SessionDB, TaskTree
import GenEq, GenBimap

//TODO: Create a better dynamic_string module with option for evaluating
//      a graph as far as possible to save a closure that is as small as
//		possible, but is still able to hold infinite structures
//

import code from "copy_graph_to_string.obj";
import code from "copy_graph_to_string_interface.obj";

:: TaskState = TSNew | TSActive | TSDone

derive gForm	TaskState
derive gUpd		TaskState
derive gPrint	TaskState
derive gParse	TaskState
derive gEq		TaskState

mkTSt :: !Lifespan !Lifespan !Session ![Workflow]!*HSt -> *TSt
mkTSt itaskstorage threadstorage session workflows hst
	=	{ taskNr		= []
		, taskInfo		= initTaskInfo
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
		, changeStack	= []
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
	# tst								= {TSt|tst	& taskNr = [0,processId], activated = True, userId = (fst user), delegatorId = (fst delegator)
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
	loadChanges` [] accu tst=:{TSt|changeStack}
		= {TSt|tst& changeStack = changeStack ++ reverse accu}
	loadChanges` [(l,c):cs] accu tst
		# (dyn,tst) = accHStTSt (getDynamic c) tst
		= case dyn of
			Just dyn	= loadChanges` cs [Just (l,c,dyn):accu] tst
			Nothing		= loadChanges` cs accu tst
	
	storeChanges pid tst=:{TSt|changeStack} = storeChanges` changeStack [] pid tst
	storeChanges` [] accu pid tst
		# (_,tst)	= updateProcess pid (\p -> {Process|p & changes = reverse accu}) tst
		= tst
	storeChanges` [Just(l,c,d):cs] accu pid tst
		| c == 0
			# (c,tst)	= createDynamic d tst
			= storeChanges` cs [(l,c):accu] pid tst
		| otherwise
			# (_,tst)	= updateDynamic d c tst
			= storeChanges` cs [(l,c):accu] pid tst
	storeChanges` [Nothing:cs] accu pid tst
		= storeChanges` cs accu pid tst
			
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
	

applyChangeToTaskTree :: !ProcessId !String !(Change a) !*TSt -> *TSt | TC a
applyChangeToTaskTree pid name change tst
	# (mbProcess,tst)		= getProcess pid tst
	= case mbProcess of
		Just proc
			# tst =:{taskNr,taskInfo,userId,delegatorId,tree,activated,mainTask,newProcesses,options,staticInfo,exception,doChange,changeStack} = tst
			# tst = snd (buildProcessTree proc {tst & doChange = True, changeStack = [Just (name,0,dynamic change)]})
			= {tst & taskNr = taskNr, taskInfo = taskInfo, userId = userId, delegatorId = delegatorId, tree = tree
				, activated = activated, mainTask = mainTask, newProcesses = newProcesses, options = options
				, staticInfo = staticInfo, exception = exception, doChange = doChange, changeStack = changeStack}
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

mkBasicTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkBasicTask taskname taskfun = Task taskname Nothing mkBasicTask`
where
	mkBasicTask` tst=:{TSt|taskNr,taskInfo}
		# (a, tst)			
			= taskfun {tst & tree = TTBasicTask taskInfo [] [] []}
		# (states, tst=:{tree = TTBasicTask info html inputs _})
			= accHStTSt (accFormStatesHSt (getHtmlStates (iTaskId taskNr "" +++ "-" ))) tst
		= (a, {tst & tree = TTBasicTask  info html inputs states})
	
mkSequenceTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkSequenceTask taskname taskfun = Task taskname Nothing mkSequenceTask`
where
	mkSequenceTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTSequenceTask taskInfo [], taskNr = [0:taskNr]}
			
mkParallelTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkParallelTask taskname taskfun = Task taskname Nothing mkParallelTask`
where
	mkParallelTask` tst=:{TSt|taskNr,taskInfo,options}
		# tst = case options.combination of
			Just combination
				= {tst & tree = TTParallelTask taskInfo combination [], taskNr = [0:taskNr], options = {options & combination = Nothing}}
			Nothing
				= {tst & tree = TTParallelTask taskInfo TTVertical [], taskNr = [0:taskNr]}												
		= taskfun tst
			
mkMainTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a
mkMainTask taskname taskfun = Task taskname Nothing mkMainTask`
where
	mkMainTask` tst=:{taskNr,taskInfo}
		= taskfun {tst & tree = TTMainTask taskInfo undef [], taskNr = [0,0:taskNr]}

applyTask :: !(Task a) !*TSt -> (!a,!*TSt) | iData a
applyTask (Task name mbCxt taskfun) tst=:{taskNr,tree=tree,options,activated, hst}
	# (store,hst) 	= mkStoreForm (Init,storageFormId options (iTaskId taskNr "") (TSNew,createDefault)) id hst
	# (state, a)	= store.Form.value
	# taskInfo =	{ taskId		= taskNrToString taskNr
					, taskLabel		= name
					, active		= activated
					, finished		= state === TSDone
					, traceValue	= printToString a
					}
	|state === TSDone || not activated
		# tst = addTaskNode (TTBasicTask taskInfo [] [] []) {tst & hst = hst}
		= (a, {tst & taskNr = incTaskNr taskNr, activated = state === TSDone})
	| otherwise
		# tst	= {tst & taskInfo = taskInfo, hst = hst}	
		// If the task is new, but has run in a different context, initialize the states of the task and its subtasks
		# tst	= initializeState state taskNr mbCxt tst
		// Execute task function
		# (a, tst=:{tree=node,activated,hst})	= taskfun tst
		# node									= updateTaskNode activated (printToString a) node
		// Update task state
		| activated
			# tst=:{hst}		= deleteTaskStates taskNr {tst & hst = hst}
			# (store,hst) 		= mkStoreForm (Init,storageFormId options (iTaskId taskNr "") (TSNew,createDefault)) (\_ -> (TSDone, a)) hst
			# tst				= addTaskNode node {tst & taskNr = incTaskNr taskNr, tree = tree, options = options, hst = hst}
			= (a, tst)
		| otherwise
			# (store,hst) 		= mkStoreForm (Init,storageFormId options (iTaskId taskNr "") (TSNew,createDefault)) (\_ -> (TSActive, a)) hst
			# tst				= addTaskNode node {tst & taskNr = incTaskNr taskNr, tree = tree, options = options, hst = hst}
			= (a, tst)			
where
	//Increase the task nr
	incTaskNr [] = [0]
	incTaskNr [i:is] = [i+1:is]
	
	initializeState TSNew taskNr (Just oldTaskNr) tst	= copyTaskStates oldTaskNr taskNr tst
	initializeState _ _ _ tst							= tst
	
	//Add a new node to the current sequence or process
	addTaskNode node tst=:{tree} = case tree of
		(TTMainTask ti mti tasks)				= {tst & tree = TTMainTask ti mti [node:tasks]}
		(TTSequenceTask ti tasks)				= {tst & tree = TTSequenceTask ti [node:tasks]}
		(TTParallelTask ti combination tasks)	= {tst & tree = TTParallelTask ti combination [node:tasks]}
		_										= {tst & tree = tree}
	
	//update the finished, tasks and traceValue fields of a task tree node
	updateTaskNode f tv (TTBasicTask ti output inputs states)	= TTBasicTask		{ti & finished = f, traceValue = tv} output inputs states
	updateTaskNode f tv (TTSequenceTask ti tasks) 				= TTSequenceTask	{ti & finished = f, traceValue = tv} (reverse tasks)
	updateTaskNode f tv (TTParallelTask ti combination tasks)	= TTParallelTask	{ti & finished = f, traceValue = tv} combination (reverse tasks)
	updateTaskNode f tv (TTMainTask ti mti tasks)				= TTMainTask		{ti & finished = f, traceValue = tv} mti (reverse tasks)		

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
		(TTSequenceTask info sequence)	= {tst & taskNr = [0:tl taskNr], tree = TTSequenceTask info []}
		_								= {tst & tree = tree}

deleteTaskStates :: !TaskNr !*TSt -> *TSt
deleteTaskStates tasknr tst = appHStTSt (deleteIData (iTaskId tasknr "")) tst

copyTaskStates :: !TaskNr !TaskNr !*TSt	-> *TSt
copyTaskStates fromtask totask tst = appHStTSt (copyIData (iTaskId fromtask "") (iTaskId totask "")) tst
			
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


taskLabel :: !(Task a) -> String
taskLabel (Task label _ _) = label
