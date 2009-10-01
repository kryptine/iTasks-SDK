implementation module TSt

import StdEnv, StdMaybe
import Http, Util
import ProcessDB, DynamicDB, SessionDB, UserDB, TaskTree
import GenPrint, GenParse, GenEq, GenBimap
import GenVisualize, GenUpdate, Store, Config

import code from "copy_graph_to_string.obj";
import code from "copy_graph_to_string_interface.obj";

:: TaskState = TSNew | TSActive | TSDone

derive gPrint		TaskState
derive gParse		TaskState
derive gEq			TaskState

mkTSt :: String Config HTTPRequest Session ![Workflow] !*Store !*World -> *TSt
mkTSt appName config request session workflows store world
	=	{ taskNr		= []
		, taskInfo		= initTaskInfo
		, firstRun		= False
		, curValue		= Nothing
		, userId		= -1
		, delegatorId	= -1
		, tree			= TTMainTask initTaskInfo initTaskProperties []
		, activated 	= True
		, mainTask		= -1
		, newProcesses	= []
		, options 		= initialOptions
		, staticInfo	= initStaticInfo appName session workflows
		, exception		= Nothing
		, doChange		= False
		, changes		= []
		, config		= config
		, request		= request
		, store			= store
		, world			= world
		}

initStaticInfo :: String Session ![Workflow] -> StaticInfo
initStaticInfo appName session workflows
	=	{ appName			= appName
		, currentProcessId	= -1
		, currentSession 	= session
		, staticWorkflows	= workflows
		}

initialOptions :: Options 
initialOptions
	=	{ trace			= False 
		, combination	= Nothing
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
	= { systemProps =
		{TaskSystemProperties
		| processId = 0
		, subject = ""
		, manager = (-1,"")
		, issuedAt = Timestamp 0
		, firstEvent = Nothing
		, latestEvent = Nothing
		}
	  , managerProps =
	    {TaskManagerProperties
	    | worker = (-1,"")
	    , priority = NormalPriority
	    , deadline = Nothing
	    }
	  , workerProps =
	    {TaskWorkerProperties
	    | progress = TPActive
	    }
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
		# (tree,tst)	= buildProcessTree p Nothing tst
		# (trees,tst)	= calculateTrees ps tst
		= ([tree:trees],tst)

calculateTaskTree	:: !ProcessId !Bool !*TSt -> (!Maybe String, !Maybe TaskTree, !*TSt)
calculateTaskTree pid enableDebug tst
	# (mbProcess,tst)		= getProcess pid tst
	= case mbProcess of
		Just entry
			= case entry.Process.status of
				Active
					# (tree,tst)	= buildProcessTree entry Nothing tst
					= (Nothing, Just tree, tst)
				_
					= (Nothing, Just (initProcessNode entry), tst)
		Nothing
			= (Just "Process not found", Nothing, tst)

initProcessNode :: Process -> TaskTree			
initProcessNode {processId, properties}
		= TTMainTask {TaskInfo|taskId = toString processId, taskLabel = properties.systemProps.subject, active = True, finished = False, traceValue = "Process"} properties []

buildProcessTree :: Process !(Maybe (Dynamic, ChangeLifeTime)) !*TSt -> (!TaskTree, !*TSt)
buildProcessTree p =: {Process | processId, processType, properties = {TaskProperties|systemProps,managerProps}, changes} mbChange tst =:{staticInfo}
	# tst								= {TSt|tst	& taskNr = [0,processId], activated = True, userId = (fst managerProps.worker), delegatorId = (fst systemProps.manager)
													, staticInfo = {StaticInfo|staticInfo & currentProcessId = processId}, tree = initProcessNode p, mainTask = processId}
	# tst								= loadChanges mbChange changes tst	
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
	loadChanges mbNew changes tst = loadChanges` mbNew changes [] tst

	loadChanges` Nothing [] accu tst=:{TSt|changes}
		= {TSt|tst& changes = reverse accu, doChange = False}
	loadChanges` (Just (change,lifetime)) [] accu tst=:{TSt|changes}
		= {TSt|tst & changes = [Just (lifetime, 0,change):changes], doChange = True}
	loadChanges` mbNew [(l,c):cs] accu tst
		# (dyn,tst) = getDynamic c tst
		= case dyn of
			Just dyn	= loadChanges` mbNew cs [Just (CLPersistent l,c,dyn):accu] tst
			Nothing		= loadChanges` mbNew cs accu tst
	
	storeChanges pid tst=:{TSt|changes} = storeChanges` changes [] pid tst
	storeChanges` [] accu pid tst
		# (_,tst)	= updateProcess pid (\p -> {Process|p & changes = reverse accu}) tst
		= tst
	storeChanges` [Just(CLPersistent l,c,d):cs] accu pid tst
		| c == 0
			# (c,tst)	= createDynamic d tst
			= storeChanges` cs [(l,c):accu] pid tst
		| otherwise
			# (_,tst)	= updateDynamic d c tst
			= storeChanges` cs [(l,c):accu] pid tst
	storeChanges` [c:cs] accu pid tst
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


applyChangeToTaskTree :: !ProcessId !Dynamic !ChangeLifeTime !*TSt -> *TSt
applyChangeToTaskTree pid change lifetime tst=:{taskNr,taskInfo,firstRun,userId,delegatorId,tree,activated,mainTask,newProcesses,options,staticInfo,exception,doChange,changes}
	# (mbProcess,tst) = getProcess pid tst
	= case mbProcess of
		(Just proc) 
			# tst = snd (buildProcessTree proc (Just (change,lifetime)) tst)
			= {tst & taskNr = taskNr, taskInfo = taskInfo, firstRun = firstRun, userId = userId, delegatorId = delegatorId
			  , tree = tree, activated = activated, mainTask = mainTask, newProcesses = newProcesses, options = options
			  , staticInfo = staticInfo, exception = exception, doChange = doChange, changes = changes}
		Nothing		
			= tst

getCurrentSession :: !*TSt 	-> (!Session, !*TSt)
getCurrentSession tst =:{staticInfo} = (staticInfo.currentSession, tst)

getCurrentUser :: !*TSt -> (!UserId, !*TSt)
getCurrentUser tst =: {staticInfo}
	= (staticInfo.currentSession.Session.user.User.userId, {tst & staticInfo = staticInfo})

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

taskFinished :: !*TSt -> (!Bool, !*TSt)
taskFinished tst=:{activated} = (activated, {tst & activated = activated})

appWorldTSt	:: !.(*World -> *World) !*TSt -> *TSt
appWorldTSt f tst=:{TSt|world}
	= {TSt|tst & world = f world}

accWorldTSt	:: !.(*World -> *(.a,*World))!*TSt -> (.a,!*TSt)
accWorldTSt f tst=:{TSt|world}
	# (a,world) = f world
	= (a, {TSt|tst & world = world})
		
mkExtJSTask	:: !String !(*TSt -> *(!a,!*TSt)) -> Task a 
mkExtJSTask taskname taskfun = Task taskname Nothing mkExtJSTask`	
where
	mkExtJSTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTExtJSTask taskInfo (abort "No ExtJS definition given")}

mkInstantTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a
mkInstantTask taskname taskfun = Task taskname Nothing mkInstantTask`
where
	mkInstantTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTFinishedTask taskInfo} //We use a FinishedTask node because the task is finished after one evaluation

mkMonitorTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a
mkMonitorTask taskname taskfun = Task taskname Nothing mkMonitorTask`
where
	mkMonitorTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTMonitorTask taskInfo []}

mkRpcTask :: !String !RPCInfo !(String -> a) -> Task a
mkRpcTask taskname info parsefun = Task taskname Nothing mkRpcTask`
where
	mkRpcTask` tst
		= (undef, tst)
	
mkSequenceTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a
mkSequenceTask taskname taskfun = Task taskname Nothing mkSequenceTask`
where
	mkSequenceTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTSequenceTask taskInfo [], taskNr = [0:taskNr]}
			
mkParallelTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a
mkParallelTask taskname taskfun = Task taskname Nothing mkParallelTask`
where
	mkParallelTask` tst=:{TSt|taskNr,taskInfo,options}
		# tst = case options.combination of
			Just combination
				= {tst & tree = TTParallelTask taskInfo combination [], taskNr = [0:taskNr], options = {options & combination = Nothing}}
			Nothing
				= {tst & tree = TTParallelTask taskInfo TTVertical [], taskNr = [0:taskNr]}												
		= taskfun tst
			
mkMainTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a
mkMainTask taskname taskfun = Task taskname Nothing mkMainTask`
where
	mkMainTask` tst=:{taskNr,taskInfo}
		= taskfun {tst & tree = TTMainTask taskInfo undef [], taskNr = [0,0:taskNr]}

applyTask :: !(Task a) !*TSt -> (!a,!*TSt) | iTask a
applyTask (Task name mbCxt taskfun) tst=:{taskNr,tree=tree,options,activated,store,world}
	# taskId				= iTaskId taskNr ""
	# (mbtv,store,world)	= loadValue taskId store world
	# (state,curval)		= case mbtv of
								(Just (state, value))	= (state, Just value)
								_						= (TSNew, Nothing)
	# taskInfo =	{ taskId		= taskNrToString taskNr
					, taskLabel		= name
					, active		= activated
					, finished		= state === TSDone
					, traceValue	= ""
					}
	# tst = {TSt|tst & store = store, world = world}
	|state === TSDone || not activated
		# tst = addTaskNode (TTFinishedTask taskInfo) tst
		= (fromJust curval, {tst & taskNr = incTaskNr taskNr, activated = state === TSDone})
	| otherwise
		# tst	= {tst & taskInfo = taskInfo, firstRun = state === TSNew, curValue = case curval of Nothing = Nothing ; Just a = Just (dynamic a)}	
		// If the task is new, but has run in a different context, initialize the states of the task and its subtasks
		# tst	= initializeState state taskNr mbCxt tst
		// Execute task function
		# (a, tst)	= taskfun tst
		// Remove user updates (needed for looping. a new task may get the same tasknr again, but should not get the events)
		# tst=:{tree=node,activated,store}	= clearUserUpdates tst
		// Update task state
		| activated
			# tst=:{TSt|store}	= deleteTaskStates taskNr {TSt|tst & store = store}
			# store				= storeValue taskId (TSDone, a) store
			# tst				= addTaskNode (TTFinishedTask taskInfo) {tst & taskNr = incTaskNr taskNr, tree = tree, options = options, store = store}
			= (a, tst)
		| otherwise
			# node				= updateTaskNode activated (printToString a) node
			# store				= storeValue taskId (TSActive, a) store
			# tst				= addTaskNode node {tst & taskNr = incTaskNr taskNr, tree = tree, options = options, store = store}
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
	updateTaskNode f tv (TTExtJSTask ti defs)					= TTExtJSTask		{ti & finished = f, traceValue = tv} defs
	updateTaskNode f tv (TTMonitorTask ti status)				= TTMonitorTask		{ti & finished = f, traceValue = tv} status
	updateTaskNode f tv (TTSequenceTask ti tasks) 				= TTSequenceTask	{ti & finished = f, traceValue = tv} (reverse tasks)
	updateTaskNode f tv (TTParallelTask ti combination tasks)	= TTParallelTask	{ti & finished = f, traceValue = tv} combination (reverse tasks)
	updateTaskNode f tv (TTMainTask ti mti tasks)				= TTMainTask		{ti & finished = f, traceValue = tv} mti (reverse tasks)		

		
setExtJSDef	:: !ExtJSDef !*TSt -> *TSt
setExtJSDef def tst=:{tree}
	= case tree of
		(TTExtJSTask info _)				= {tst & tree = TTExtJSTask info (Left def)}
		_									= tst

setExtJSUpdates :: ![ExtJSUpdate] !*TSt -> *TSt
setExtJSUpdates upd tst=:{tree}
	= case tree of
		(TTExtJSTask info _)				= {tst & tree = TTExtJSTask info (Right upd)}
		_									= tst

setStatus :: ![HtmlTag] !*TSt -> *TSt
setStatus msg tst=:{tree}
	= case tree of
		(TTMonitorTask info _)				= {tst & tree = TTMonitorTask info msg}
		_									= tst

getTaskValue :: !*TSt -> (Maybe a, !*TSt) | TC a
getTaskValue tst=:{curValue = Just (a :: a^)} = (Just a, tst)
getTaskValue tst = (Nothing, tst)

setTaskStore :: !String !a !*TSt -> *TSt | iTask a
setTaskStore key value tst=:{taskNr,store}
	# store = storeValue storekey value store
	= {TSt|tst & store = store}
where
	storekey = taskNrToString taskNr +++ "-" +++ key

getTaskStore :: !String !*TSt -> (Maybe a, !*TSt) | iTask a
getTaskStore key tst=:{taskNr,store,world}
	# (mbValue,store,world) = loadValue storekey store world
	= (mbValue,{TSt|tst&store = store, world = world})
where
	storekey = taskNrToString taskNr +++ "-" +++ key

getUserUpdates :: !*TSt -> ([(String,String)],!*TSt)
getUserUpdates tst=:{taskNr,request} = (updates request, tst);
where
	updates request
		| http_getValue "_targettask" request.arg_post "" == taskNrToString taskNr
			= [u \\ u =:(k,v) <- request.arg_post | k.[0] <> '_']
		| otherwise
			= []
clearUserUpdates	:: !*TSt						-> *TSt
clearUserUpdates tst=:{taskNr, request}
	| http_getValue "_targettask" request.arg_post "" == taskNrToString taskNr
		= {tst & request = {request & arg_post = [u \\ u =:(k,v) <- request.arg_post | k.[0] == '_']}}
	| otherwise
		= tst
		
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
deleteTaskStates tasknr tst=:{TSt|store,world}
	# (store,world) = deleteValues (iTaskId tasknr "") store world
	= {TSt|tst & store = store, world = world}
	
copyTaskStates :: !TaskNr !TaskNr !*TSt	-> *TSt
copyTaskStates fromtask totask tst=:{TSt|store,world}
	# (store,world) = copyValues (iTaskId fromtask "") (iTaskId totask "") store world
	= {TSt|tst & store = store, world = world}

flushStore :: !*TSt -> *TSt
flushStore tst=:{TSt|store,world}
	# (store,world) = flushCache store world
	= {TSt|tst & store = store, world = world}

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