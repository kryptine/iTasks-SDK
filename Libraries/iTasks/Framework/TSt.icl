implementation module TSt

import StdEnv, StdMaybe
import Http, Util
import ProcessDB, DynamicDB, SessionDB, UserDB, TaskTree
import GenPrint, GenParse, GenEq, GenBimap
import GenVisualize, GenUpdate, Store, Config

import dynamic_string

from JSON 				import JSONDecode, fromJSON

import code from "copy_graph_to_string.obj";
import code from "copy_graph_to_string_interface.obj";

:: TaskState = TSNew | TSActive | TSDone

:: RPCMessage = 
		{ success		:: Bool
		, resultChange	:: Bool
		, finished		:: Bool
		, status		:: String
		, result		:: String
		}

derive gPrint		TaskState
derive gParse		TaskState
derive gEq			TaskState

derive JSONDecode RPCMessage

mkTSt :: String Config HTTPRequest Session ![Workflow] !*Store !*Store !*World -> *TSt
mkTSt appName config request session workflows systemStore dataStore world
	=	{ taskNr		= []
		, taskInfo		= initTaskInfo
		, firstRun		= False
		, curValue		= Nothing
		, userId		= -1
		, delegatorId	= -1
		, tree			= TTMainTask initTaskInfo initTaskProperties []
		, activated 	= True
		, mainTask		= ""
		, newProcesses	= []
		, options 		= initialOptions
		, staticInfo	= initStaticInfo appName session workflows
		, exception		= Nothing
		, doChange		= False
		, changes		= []
		, config		= config
		, request		= request
		, systemStore	= systemStore
		, dataStore		= dataStore
		, world			= world
		}

initStaticInfo :: String Session ![Workflow] -> StaticInfo
initStaticInfo appName session workflows
	=	{ appName			= appName
		, currentProcessId	= ""
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
		| processId = ""
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
	# (processes,tst)		= getProcessesForUser currentUser [Active] tst	//Lookup all active processes for this user
	# (trees,tst)			= calculateTrees (sortProcesses processes) tst
	# (trees,tst)			= addNewProcesses (reverse trees) tst
	= (Nothing, trees, tst)	

//Needed for RPC Daemon
calculateCompleteTaskForest :: !Bool !*TSt -> (Maybe String, ![TaskTree], !*TSt)
calculateCompleteTaskForest enableDebug tst 
	# (processes, tst) = getProcesses [Active,Suspended] tst
	# (trees, tst) 	   = calculateTrees processes tst
	# (trees, tst)	   = addNewProcesses trees tst
	= (Nothing, trees, tst)

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
	# (tree, par, tst)	= buildProcessTree p Nothing tst
	= case par of 
		(Just procId)
			# (mbProc,tst) 	= getProcess procId tst
			| isJust mbProc	
				# (trees,tst)	= calculateTrees [fromJust mbProc:ps] tst
				= ([tree:trees],tst)
			| otherwise	
				# (trees,tst)	= calculateTrees ps tst
				= ([tree:trees],tst)
		Nothing
			# (trees,tst)		= calculateTrees ps tst
			= ([tree:trees],tst)

calculateTaskTree	:: !ProcessId !Bool !*TSt -> (!Maybe String, !Maybe TaskTree, !*TSt)
calculateTaskTree pid enableDebug tst
	# (mbProcess,tst) = getProcess pid tst
	= case mbProcess of
		Just entry
			= case entry.Process.status of
				Active
					# (tree,_,tst)	= buildProcessTree entry Nothing tst
					= (Nothing, Just tree, tst)
				_
					= (Nothing, Just (initProcessNode entry), tst)
		Nothing
			= (Just "Process not found", Nothing, tst)

initProcessNode :: Process -> TaskTree			
initProcessNode {processId, properties}
		= TTMainTask {TaskInfo|taskId = toString processId, taskLabel = properties.systemProps.subject, active = True, finished = False, traceValue = "Process"} properties []

//If parent has to be evaluated, the Id is returned and accumelated into the list of proccesses still to be evaluated in buildtree.
buildProcessTree :: Process !(Maybe (Dynamic, ChangeLifeTime)) !*TSt -> (!TaskTree, (Maybe ProcessId), !*TSt)
buildProcessTree p =: {Process | processId, parent, properties = {TaskProperties|systemProps,managerProps}, changes, changeNr} mbChange tst =:{taskNr,staticInfo}
	//Init tst
	# tst									= {TSt|tst & taskNr = [changeNr:taskNrFromString processId], activated = True, userId = (fst managerProps.worker), delegatorId = (fst systemProps.manager)
												, staticInfo = {StaticInfo|staticInfo & currentProcessId = processId}, tree = initProcessNode p, mainTask = processId}
	# tst									= loadChanges mbChange changes tst
	# (result, tst)							= applyMainTask tst
	# (TTMainTask ti mti tasks, tst)		= getTaskTree tst
	# (finished, tst)						= taskFinished tst
	| finished
		# tst 			= storeProcessResult (taskNrFromString processId) result tst
		# (_,tst)		= updateProcess processId (\p -> {Process|p & status = Finished}) tst
		= (TTFinishedTask {TaskInfo | ti & finished = True}, (parentProcId parent), tst)
	| otherwise
		# tst							= storeChanges processId tst
		= (TTMainTask ti mti tasks, Nothing, tst)
		
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

	applyMainTask tst=:{taskNr}
		# (thread, tst)		= loadTaskThread (taskNrFromString processId) tst		  
		# (result, tst) 	= thread tst
		#  result			= evalDynamicResult result
		= (result,tst)
		
	parentProcId ""		=  Nothing
	parentProcId procId = (Just procId)


/*
//Turn a task yielding a value of type a into a value of dynamic
createDynamicTask :: !(Task a) -> Task Dynamic | iTask a
createDynamicTask (Task name mbCxt tf) = Task name mbCxt createDynamicTask`
where
	createDynamicTask` tst
		# (a, tst)	= tf tst
		# dyn		= evalDynamicResult (dynamic a)
		= (dyn, tst)
*/

createTaskThread :: !(Task a) -> (!*TSt -> *(!Dynamic,!*TSt)) | iTask a
createTaskThread task = createTaskThread` task
where
	createTaskThread` :: !(Task a) !*TSt -> *(!Dynamic, !*TSt) | iTask a
	createTaskThread` task tst
		# (a, tst)	= applyTask task tst
		# dyn		= evalDynamicResult (dynamic a)
		= (dyn,tst)

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

applyChangeToTaskTree :: !ProcessId !Dynamic !ChangeLifeTime !*TSt -> *TSt
applyChangeToTaskTree pid change lifetime tst=:{taskNr,taskInfo,firstRun,userId,delegatorId,tree,activated,mainTask,newProcesses,options,staticInfo,exception,doChange,changes}
	# (mbProcess,tst) = getProcess pid tst
	= case mbProcess of
		(Just proc) 
			# tst = thd3 (buildProcessTree proc (Just (change,lifetime)) tst)
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
		
mkInteractiveTask	:: !String !(*TSt -> *(!a,!*TSt)) -> Task a 
mkInteractiveTask taskname taskfun = Task taskname Nothing mkInteractiveTask`	
where
	mkInteractiveTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTInteractiveTask taskInfo (abort "No interface definition given")}

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

mkRpcTask :: !String !RPCInfo !(String -> a) -> Task a | gUpdate{|*|} a
mkRpcTask taskname rpci parsefun = Task taskname Nothing mkRpcTask`
where
	mkRpcTask` tst=:{TSt | taskNr, taskInfo}
		# rpci				= {RPCInfo | rpci & taskId = taskNrToString taskNr}
		# (updates, tst) 	= getRpcUpdates tst
		# (rpci, tst) 		= checkRpcStatus rpci tst
		| length updates == 0 
			= applyRpcDefault {tst & activated = False, tree = TTRpcTask taskInfo rpci }					
		| otherwise 
			= applyRpcUpdates updates tst rpci parsefun
	
	checkRpcStatus :: RPCInfo !*TSt -> (!RPCInfo, !*TSt)
	checkRpcStatus rpci tst 
		# (mbStatus, tst) = getTaskStore "status" tst 
		= case mbStatus of
		Nothing 
			# tst = setTaskStore "status" "Pending" tst
			= ({RPCInfo | rpci & status = "Pending"},tst)
		Just s
			= ({RPCInfo | rpci & status = s},tst)
	
	getRpcUpdates :: !*TSt -> ([(String,String)],!*TSt)
	getRpcUpdates tst=:{taskNr,request} = (updates request, tst)
	where
		updates request
			| http_getValue "_rpctaskid" request.arg_post "" == taskNrToString taskNr
				= [u \\ u =: (k,v) <- request.arg_post]
			| otherwise
				= []

import StdDebug

/* Error handling needs to be implemented! */	
applyRpcUpdates :: [(String,String)] !*TSt !RPCInfo !(String -> a) -> *(!a,!*TSt) | gUpdate{|*|} a	
applyRpcUpdates [] tst rpci parsefun = applyRpcDefault tst
applyRpcUpdates [(n,v):xs] tst rpci parsefun
| n == "_rpcmessage" 
# (mbMsg) = fromJSON v
= case mbMsg of
	Just msg = applyRpcMessage msg tst rpci parsefun
	Nothing = abort("Cannot parse daemon message "+++v) //needs to be exception!
| otherwise = applyRpcUpdates xs tst rpci parsefun
where
	applyRpcMessage msg tst rpci parsfun
	# tst = (setTaskStore "status" msg.RPCMessage.status tst)
	= case msg.RPCMessage.success of
		True
		# tst = checkFinished msg.RPCMessage.finished tst
		| msg.RPCMessage.resultChange = (parsefun msg.RPCMessage.result, tst)
		| otherwise = applyRpcDefault tst
		False
		# tst = {TSt | tst & activated = True}
		= applyRpcDefault tst
	
	checkFinished True 	tst = {TSt | tst & activated = True}
	checkFinished False tst = {TSt | tst & activated = False}

applyRpcDefault :: !*TSt -> *(!a,!*TSt) | gUpdate{|*|} a
applyRpcDefault tst=:{TSt|world}
	# (def,wrld) = defaultValue world
	= (def,{TSt | tst & world=wrld})
	
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
		= taskfun {tst & tree = TTMainTask taskInfo undef []}

applyTask :: !(Task a) !*TSt -> (!a,!*TSt) | iTask a
applyTask (Task name mbCxt taskfun) tst=:{taskNr,tree=tree,options,activated,dataStore,world}
	# taskId				= taskNrToString taskNr
	# (mbtv,dstore,world)	= loadValue taskId dataStore world
	# (state,curval)		= case mbtv of
								(Just (state, value))	= (state, Just value)
								_						= (TSNew, Nothing)
	# taskInfo =	{ taskId		= taskNrToString taskNr
					, taskLabel		= name
					, active		= activated
					, finished		= state === TSDone
					, traceValue	= ""
					}
	# tst = {TSt|tst & dataStore = dstore, world = world}
	|state === TSDone || not activated
		# tst = addTaskNode (TTFinishedTask {taskInfo & traceValue = printToString(fromJust curval)}) tst
		= (fromJust curval, {tst & taskNr = incTaskNr taskNr, activated = state === TSDone})
	| otherwise
		# tst	= {tst & taskInfo = taskInfo, firstRun = state === TSNew, curValue = case curval of Nothing = Nothing ; Just a = Just (dynamic a)}	
		// If the task is new, but has run in a different context, initialize the states of the task and its subtasks
		# tst	= initializeState state taskNr mbCxt tst
		// Execute task function
		# (a, tst)	= taskfun tst
		// Remove user updates (needed for looping. a new task may get the same tasknr again, but should not get the events)
		# tst=:{tree=node,activated,dataStore}	= clearUserUpdates tst
		// Update task state
		| activated
			# tst=:{TSt|dataStore}	= deleteTaskStates taskNr {TSt|tst & dataStore = dataStore}
			# dataStore				= storeValue taskId (TSDone, a) dataStore
			# tst					= addTaskNode (TTFinishedTask {taskInfo & traceValue = printToString a}) {tst & taskNr = incTaskNr taskNr, tree = tree, options = options, dataStore = dataStore}
			= (a, tst)
		| otherwise
			# node				= updateTaskNode activated (printToString a) node
			# dataStore			= storeValue taskId (TSActive, a) dataStore
			# tst				= addTaskNode node {tst & taskNr = incTaskNr taskNr, tree = tree, options = options, dataStore = dataStore}
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
	updateTaskNode f tv (TTInteractiveTask ti defs)				= TTInteractiveTask	{ti & finished = f, traceValue = tv} defs
	updateTaskNode f tv (TTMonitorTask ti status)				= TTMonitorTask		{ti & finished = f, traceValue = tv} status
	updateTaskNode f tv (TTSequenceTask ti tasks) 				= TTSequenceTask	{ti & finished = f, traceValue = tv} (reverse tasks)
	updateTaskNode f tv (TTParallelTask ti combination tasks)	= TTParallelTask	{ti & finished = f, traceValue = tv} combination (reverse tasks)
	updateTaskNode f tv (TTMainTask ti mti tasks)				= TTMainTask		{ti & finished = f, traceValue = tv} mti (reverse tasks)		
	updateTaskNode f tv (TTRpcTask ti rpci)						= TTRpcTask			{ti & finished = f, traceValue = tv} rpci
		
setTUIDef	:: !TUIDef !*TSt -> *TSt
setTUIDef def tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)			= {tst & tree = TTInteractiveTask info (Left def)}
		_									= tst

setTUIUpdates :: ![TUIUpdate] !*TSt -> *TSt
setTUIUpdates upd tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)			= {tst & tree = TTInteractiveTask info (Right upd)}
		_									= tst

setStatus :: ![HtmlTag] !*TSt -> *TSt
setStatus msg tst=:{tree}
	= case tree of
		(TTMonitorTask info _)				= {tst & tree = TTMonitorTask info msg}
		_									= tst

getTaskValue :: !*TSt -> (Maybe a, !*TSt) | TC a
getTaskValue tst=:{curValue = Just (a :: a^)} = (Just a, tst)
getTaskValue tst = (Nothing, tst)

loadTaskFunctionStatic :: !TaskNr !*TSt -> (!Maybe (Task a), !*TSt) | TC a
loadTaskFunctionStatic taskNr tst =: {TSt | dataStore, world}
# (mbDyn, dataStore, world) = loadValue (storekey taskNr) dataStore world
= case mbDyn of 
	(Just (t :: Task a^)) 	= ((Just t), {TSt | tst & dataStore = dataStore, world = world})
	Nothing				  	= (Nothing , {TSt | tst & dataStore = dataStore, world = world})
where
	storekey taskNr  	 = "iTask_"+++(taskNrToString taskNr)+++"-taskfun-static"

loadTaskFunctionDynamic :: !TaskNr !*TSt -> (!Maybe (Task Dynamic), !*TSt)
loadTaskFunctionDynamic taskNr tst =: {TSt | dataStore, world}
# (mbDyn, dataStore, world) = loadValue (storekey taskNr) dataStore world
= case mbDyn of 
	(Just (t :: Task Dynamic)) 	= ((Just t), {TSt | tst & dataStore = dataStore, world = world})
	Nothing				  		= (Nothing , {TSt | tst & dataStore = dataStore, world = world})
where
	storekey taskNr 	 = "iTask_"+++(taskNrToString taskNr)+++"-taskfun-dynamic"

storeTaskFunctionStatic :: !TaskNr !(Task a) !*TSt -> *TSt | TC a
storeTaskFunctionStatic taskNr task tst = storeTaskFunction taskNr task "static" tst

storeTaskFunctionDynamic :: !TaskNr !(Task Dynamic) !*TSt -> *TSt
storeTaskFunctionDynamic taskNr task tst = storeTaskFunction taskNr task "dynamic" tst

storeTaskFunction :: !TaskNr !(Task a) String !*TSt -> *TSt | TC a
storeTaskFunction taskNr task key tst =: {TSt | dataStore}
# dataStore = storeValueAs SFPlain (storekey taskNr key) (dynamic task) dataStore
= {TSt | tst & dataStore = dataStore}
where
	storekey taskNr key = "iTask_"+++(taskNrToString taskNr)+++"-taskfun-"+++key 

storeTaskThread :: !TaskNr !(!*TSt -> *(!Dynamic,!*TSt)) !*TSt -> *TSt
storeTaskThread taskNr thread tst =:{dataStore}
	# dataStore = storeValueAs SFDynamic key (dynamic thread :: !*TSt -> *(!Dynamic,!*TSt)) dataStore
	= {TSt | tst & dataStore = dataStore}
where
	key = "iTask_" +++ (taskNrToString taskNr) +++ "-thread"


loadTaskThread :: !TaskNr !*TSt -> (!*TSt -> *(!Dynamic,!*TSt), !*TSt)
loadTaskThread taskNr tst =:{dataStore,world}
	# (mbDyn, dataStore, world)	= loadValue key dataStore world
	= case mbDyn of
		(Just (f :: !*TSt -> *(!Dynamic, !*TSt)))
			= (f, {TSt | tst & dataStore = dataStore, world = world})
		(Just _)
			= abort ("(loadTaskThread) Failed to match thread for " +++ taskNrToString taskNr)
		Nothing
			= abort ("(loadTaskThread) Failed to load thread for " +++ taskNrToString taskNr)	
where
	key = "iTask_" +++ (taskNrToString taskNr) +++ "-thread"


/**
* Store and load the result of a workflow instance
*/
loadProcessResult :: !TaskNr !*TSt -> (!Maybe a, !*TSt) | TC a
loadProcessResult taskNr tst =:{dataStore, world}
	# (mbDyn, dataStore, world) = loadValue key dataStore world
	= case mbDyn of
		( Just (result :: a^))	= (Just result, {TSt | tst & dataStore = dataStore, world = world})
		Nothing					= (Nothing, {TSt | tst & dataStore = dataStore, world = world})
where
	key = "iTask_"+++(taskNrToString taskNr)+++"-result"
	
storeProcessResult :: !TaskNr !Dynamic !*TSt -> *TSt
storeProcessResult taskNr result tst=:{dataStore}
	# dataStore	= storeValueAs SFDynamic key result dataStore
	= {TSt |tst & dataStore = dataStore}
where
	key = "iTask_"+++(taskNrToString taskNr)+++"-result"
	
setTaskStore :: !String !a !*TSt -> *TSt | iTask a
setTaskStore key value tst=:{taskNr,dataStore}
	# dataStore = storeValue storekey value dataStore
	= {TSt|tst & dataStore = dataStore}
where
	storekey = "iTask_" +++ (taskNrToString taskNr) +++ "-" +++ key

getTaskStore :: !String !*TSt -> (Maybe a, !*TSt) | iTask a
getTaskStore key tst=:{taskNr,dataStore,world}
	# (mbValue,dataStore,world) = loadValue storekey dataStore world
	= (mbValue,{TSt|tst&dataStore = dataStore, world = world})
where
	storekey = "iTask_" +++ (taskNrToString taskNr) +++ "-" +++ key

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
deleteTaskStates tasknr tst=:{TSt|dataStore,world}
	# (dstore,world) = deleteValues (iTaskId tasknr "") dataStore world
	= {TSt|tst & dataStore = dstore, world = world}
	
copyTaskStates :: !TaskNr !TaskNr !*TSt	-> *TSt
copyTaskStates fromtask totask tst=:{TSt|dataStore,world}
	# (dstore,world) = copyValues (iTaskId fromtask "") (iTaskId totask "") dataStore world
	= {TSt|tst & dataStore = dstore, world = world}

flushStore :: !*TSt -> *TSt
flushStore tst=:{TSt|dataStore,systemStore,world}
	# (dstore,world) = flushCache dataStore world
	# (sstore,world) = flushCache systemStore world
	= {TSt|tst & dataStore = dstore, systemStore = sstore, world = world}

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

taskLabel :: !(Task a) -> String
taskLabel (Task label _ _) = label