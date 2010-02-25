implementation module TSt

import StdEnv, StdMaybe
import Http, Util
import ProcessDB, SessionDB, DocumentDB, UserDB, TaskTree
import CommonDomain

import GenPrint, GenParse, GenEq
import GenVisualize, GenUpdate, Store, Config

import dynamic_string

from JSON import JSONDecode, fromJSON

:: RPCMessage =
			{ success		:: Bool
			, error			:: Bool
			, finished		:: Bool
			, result		:: String
			, status		:: String
			, errormsg		:: String
			}

derive gPrint		TaskResult
derive gParse		TaskResult

derive bimap 		Maybe, (,)
derive JSONDecode RPCMessage

mkTSt :: String Config HTTPRequest Session ![Workflow] !*Store !*Store !*Store !*World -> *TSt
mkTSt appName config request session workflows systemStore dataStore fileStore world
	=	{ taskNr		= []
		, taskInfo		= initTaskInfo
		, firstRun		= False
		, userId		= ""
		, delegatorId	= ""
		, tree			= TTMainTask initTaskInfo initTaskProperties []
		, mainTask		= ""
		, staticInfo	= initStaticInfo appName session workflows
		, doChange		= False
		, changes		= []
		, config		= config
		, request		= request
		, systemStore	= systemStore
		, dataStore		= dataStore
		, documentStore	= fileStore
		, world			= world
		}

initStaticInfo :: String Session ![Workflow] -> StaticInfo
initStaticInfo appName session workflows
	=	{ appName			= appName
		, currentProcessId	= ""
		, currentSession 	= session
		, staticWorkflows	= workflows
		}

initTaskInfo :: TaskInfo
initTaskInfo
	=	{ TaskInfo
		| taskId = ""
		, taskLabel = ""
		, traceValue = ""
		}

initTaskProperties :: TaskProperties
initTaskProperties
	= { systemProps =
		{TaskSystemProperties
		| processId = ""
		, manager = ("","")
		, issuedAt = Timestamp 0
		, firstEvent = Nothing
		, latestEvent = Nothing
		, latestExtEvent = Nothing
		}
	  , managerProps =
	    {TaskManagerProperties
	    | worker = ("","")
	    , subject = ""
	    , priority = NormalPriority
	    , deadline = Nothing
	    }
	  , workerProps =
	    {TaskWorkerProperties
	    | progress = TPActive
	    }
	 }
	  
createTaskInstance :: !(Task a) !TaskManagerProperties !Bool !*TSt -> (!TaskResult a,!ProcessId,!*TSt) | iTask a
createTaskInstance task managerProps toplevel tst=:{taskNr,mainTask}
	# (manager,tst)			= getCurrentUser tst
	# (currentTime, tst)	= accWorldTSt time tst
	# processId				= if toplevel "" (taskNrToString taskNr)
	# parent				= if toplevel "" mainTask
	# properties =
		{TaskProperties
		| systemProps =
			{TaskSystemProperties
			| processId	= ""
			, manager		= (manager.User.userName, manager.User.displayName)
			, issuedAt	= currentTime
			, firstEvent	= Nothing
			, latestEvent	= Nothing
			, latestExtEvent = Nothing
			}
		, managerProps = managerProps
		, workerProps =
			{TaskWorkerProperties
			| progress	= TPActive
			}
		}
	# process =
		{ Process
		| processId		= processId
		, status		= Active
		, parent		= parent
		, properties	= properties
		, changes		= []
		, changeCount	= 0
		, menus			= Nothing
		}
	//Create an entry in the process table
	# (processId, tst)		= createProcess process tst
	//Store the task as dynamic
	# tst					= storeTaskFunctionStatic (taskNrFromString processId) task tst
	//Store the runnable theread
	# tst					= storeTaskThread (taskNrFromString processId) (createTaskThread task) tst	
	//Evaluate the process once to kickstart automated steps that can be set in motion immediately
	# (result,tree,tst)		= evaluateTaskInstance {Process|process & processId = processId} Nothing toplevel {tst & staticInfo = {tst.staticInfo & currentProcessId = processId}}
	= case result of
		TaskBusy				= (TaskBusy, processId, tst)
		TaskFinished (a :: a^)	= (TaskFinished a, processId, tst)
		TaskException e			= (TaskException e, processId, tst)
		
//Computes a workflow (sub) process
evaluateTaskInstance :: !Process !(Maybe ChangeInjection) !Bool !*TSt-> (!TaskResult Dynamic, !TaskTree, !*TSt)
evaluateTaskInstance process=:{Process | processId, parent, properties, changes, changeCount} mbChange isTop tst
	//TODO: First apply change magic
	//Reset the task state
	# tst					= resetTSt processId properties tst
	//Load the task instance 
	# (thread,tst)			= loadTaskThread (taskNrFromString processId) tst
	//Evaluate the task instance
	# (result,tst)			= thread tst
	# (tree,tst)			= getTaskTree tst
	= case result of
		TaskBusy
			= (TaskBusy, tree, tst)
		TaskFinished dyn
			| isTop
				//Store result
				# tst 		= storeProcessResult (taskNrFromString processId) result tst
				//Update process table
				# (_,tst)	= updateProcess processId (\p -> {Process|p & status = Finished}) tst
				//Evaluate parent process
				| parent <> ""
					# (mbParentProcess,tst) = getProcess parent tst
					= case mbParentProcess of
						Nothing 
							= (result,tree,tst)
						Just parentProcess
							# (_,_,tst)	= evaluateTaskInstance parentProcess Nothing True tst
							= (result,tree,tst)	
				| otherwise
					= (result,tree,tst)
			| otherwise
				//Just return the result and tree. No need to save it.
				= (result,tree,tst)
		TaskException e
			//Store exception
			# tst 		= storeProcessResult (taskNrFromString processId) result tst
			//Update process table
			# (_,tst)	= updateProcess processId (\p -> {Process|p & status = Excepted}) tst
			= (TaskException e, tree, tst)
where
	resetTSt processId properties tst
		# taskNr	= taskNrFromString processId
		# tree		= TTMainTask {TaskInfo|taskId = toString processId, taskLabel = properties.managerProps.subject, traceValue = ""} properties []
		= {TSt| tst & taskNr = taskNr, tree = tree, staticInfo = {tst.staticInfo & currentProcessId = processId}}

calculateTaskTree :: !ProcessId !*TSt -> (!TaskTree, !*TSt)
calculateTaskTree processId tst
	# (mbProcess,tst) = getProcess processId tst
	= case mbProcess of
		Nothing
			= (TTFinishedTask {TaskInfo|taskId = toString processId, taskLabel = "Deleted Process", traceValue="Deleted"}, tst)
		Just process=:{Process|status,properties}
			= case status of
				Active
					//Evaluate the process
					# (result,tree,tst) = evaluateTaskInstance process Nothing True tst
					= (tree,tst)
				_		
					= (TTFinishedTask {TaskInfo|taskId = toString processId, taskLabel = properties.managerProps.subject, traceValue = "Finished"}, tst)

calculateTaskForest :: !*TSt -> (![TaskTree], !*TSt)
calculateTaskForest tst 
	# (processes, tst) = getProcesses [Active] tst
	= calculateTrees [processId \\ {Process|processId} <- processes | isTopLevel processId] tst
where
	isTopLevel p	= length (taskNrFromString p) == 1
	
	calculateTrees []     tst = ([],tst)
	calculateTrees [p:ps] tst
		# (tree,tst)	= calculateTaskTree p tst
		# (trees,tst)	= calculateTrees ps tst
		= ([tree:trees],tst)

/*
//If parent has to be evaluated, the Id is returned and accumulated into the list of proccesses still to be evaluated in buildtree.
buildProcessTree :: Process !(Maybe (Dynamic, ChangeLifeTime)) !*TSt -> (!TaskTree,!Bool,!*TSt)
buildProcessTree p =: {Process | processId, parent, properties = {TaskProperties|systemProps,managerProps}, changes, changeCount} mbChange tst =:{taskNr,staticInfo}

	# tst								= {TSt|tst & taskNr = [changeCount:taskNrFromString processId], userId = (fst managerProps.worker), delegatorId = (fst systemProps.manager)
											, staticInfo = {StaticInfo|staticInfo & currentProcessId = processId}, tree = initProcessNode p, mainTask = processId}
	# tst								= loadChanges mbChange changes tst
	# (result,tst)						= executeTaskThread tst
	# (TTMainTask ti mti tasks, tst)	= getTaskTree tst
	= case result of
		TaskFinished dyn
			# tst 			= storeProcessResult (taskNrFromString processId) dyn tst
			# (_,tst)		= updateProcess processId (\p -> {Process|p & status = Finished}) tst
			= (TTFinishedTask ti, True, tst)
		_
			# tst			= storeChanges processId tst
			= (TTMainTask ti mti tasks, False, tst)	
where	
	initProcessNode {Process|processId, properties}
		= TTMainTask {TaskInfo|taskId = toString processId, taskLabel = properties.managerProps.subject, traceValue = "Process"} properties []
	
	loadChanges mbNew changes tst = loadChanges` mbNew changes [] tst

	loadChanges` Nothing [] accu tst=:{TSt|changes}
		= {TSt|tst& changes = reverse accu, doChange = False}
	loadChanges` (Just (change,lifetime)) [] accu tst=:{TSt|changes}
		= {TSt|tst & changes = [Just (lifetime, 0,change):changes], doChange = True}
	loadChanges` mbNew [(l,c):cs] accu tst
		# (dyn,tst) = (abort "getDynamic",tst)//TODO getDynamic c tst
		= case dyn of
			Just dyn	= loadChanges` mbNew cs [Just (CLPersistent l,c,dyn):accu] tst
			Nothing		= loadChanges` mbNew cs accu tst
	
	storeChanges pid tst=:{TSt|changes} = storeChanges` changes [] pid tst
	storeChanges` [] accu pid tst
		# (_,tst)	= updateProcess pid (\p -> {Process|p & changes = [] /* TODO reverse accu */}) tst
		= tst
	storeChanges` [Just(CLPersistent l,cid):cs] accu pid tst
	
		//| c == 0
		//	# (c,tst)	= (abort "createDynamic",tst) //TODO createDynamic d tst
		//	= storeChanges` cs [(l,c):accu] pid tst
		| otherwise
			# (_,tst)	= (abort "updateDynamic", tst) //TODO updateDynamic d c tst
			= storeChanges` cs [(l,cid):accu] pid tst
		
	storeChanges` [c:cs] accu pid tst
		= storeChanges` cs accu pid tst

	executeTaskThread tst=:{taskNr}
		# (thread, tst)			= loadTaskThread (taskNrFromString processId) tst		  
		# (result,tst)		= thread tst
		= (result,tst)
*/

//OUDE IMPLEMENTATIE VAN ASSIGN
/*
assign` :: !UserName !TaskPriority !(Maybe Timestamp) !(Task a) !*TSt -> (!TaskResult a, !*TSt) | iTask a
assign` toUserName initPriority initDeadline task tst =: { TSt| taskNr, taskInfo, firstRun, mainTask = currentMainTask, staticInfo = {currentProcessId}
													   , userId, delegatorId = currentDelegatorId, doChange, changes, dataStore, world}
	# taskId  			   = taskNrToString taskNr
	# (mbProc,tst) 		   = getProcess taskId tst
	# (taskStatus, taskProperties, curTask, changeCount, tst)
		= case mbProc of
			(Just {Process | status, properties, changeCount})
				# (curTask,tst) = loadTaskFunctionStatic taskNr tst
				| isNothing curTask
					= abort ("(assign) No task function stored for process " +++ taskNrToString taskNr)
				| otherwise
					= (status, properties, fromJust curTask, changeCount, tst)		
			Nothing
				# (user,tst)		= getUser toUserName tst
				# initProperties
					= {TaskManagerProperties
					  | worker		= (user.User.userName, user.User.displayName)
					  , subject		= taskLabel task
					  , priority	= initPriority
					  , deadline	= initDeadline
					}
				# (processId,tst)	= createTaskInstance task initProperties False tst
				# (mbProc,tst)		= getProcess processId tst
				= case mbProc of
					(Just {Process | status, properties, changeCount})
						= (status, properties, task, changeCount, tst)
					_
						= abort "(assign) Could not load newly created process"
				
	//Process has finished, unpack the dynamic result
	| taskStatus == Finished
		# (mbRes,tst) = loadProcessResult taskNr tst
		= case mbRes of
			Just (TaskFinished a)	= (a, tst)
			Nothing					= abort "(assign) No process result stored"
			_						= abort "(assign) Could not unpack process result"
	//Apply all active changes (oldest change first, hence the 'reverse changes')
	| firstRun
		= all_changes taskNr taskInfo taskProperties changeCount task curTask (reverse changes) {TSt|tst & changes = []}
	//Apply the current change change
	| doChange
		= case changes of
			[Just (clt,cid,cdyn):rest]
				= one_change taskNr taskInfo taskProperties changeCount task curTask (clt,cid,cdyn) rest tst
			other
				= do_task taskNr taskInfo taskProperties changeCount curTask tst
	| otherwise
		= do_task taskNr taskInfo taskProperties changeCount curTask tst
*/
/*
//Just execute the task
all_changes :: !TaskNr !TaskInfo !TaskProperties !Int !(Task a) !(Task a) ![Maybe (!ChangeLifeTime,!DynamicId,!Dynamic)] !*TSt -> (!TaskResult a,!*TSt) | iTask a
all_changes taskNr taskInfo taskProperties changeCount origTask curTask [] tst
	= do_task taskNr taskInfo taskProperties changeCount curTask tst
	
all_changes taskNr taskInfo taskProperties changeCount origTask curTask [Just (clt,cid,cdyn):cs] tst=:{TSt|changes}
	# processId = taskNrToString taskNr
	# (mbProperties,mbTask,mbChange) = appChange cdyn taskProperties curTask origTask
	# changes = case mbChange of
		(Just change)	= [Just (clt,cid,change):changes]
		Nothing			= [Nothing:changes]
	//Update task (and properties when changed) 	
	| isJust mbTask
		# changeCount		= inc changeCount 
		# taskProperties	= if (isJust mbProperties) (fromJust mbProperties) taskProperties
		# curTask			= fromJust mbTask				
		# tst				= storeTaskThread taskNr (createTaskThread curTask) tst
		# tst				= storeTaskFunctionStatic taskNr curTask tst
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties, changeCount = changeCount}) tst
		# (a,tst)			= do_task taskNr taskInfo taskProperties changeCount curTask {TSt|tst & changes = changes}
		= case cs of
			[]	-> (a,tst)
			_	-> all_changes taskNr taskInfo taskProperties changeCount origTask curTask cs {TSt|tst & changes = changes}
	//Only add properties
	| isJust mbProperties
		# taskProperties	= fromJust mbProperties
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties}) tst
		# (a,tst) 			= do_task taskNr taskInfo taskProperties changeCount curTask {TSt|tst & changes = changes}
		= case cs of
			[]	= (a,tst)
			_	= all_changes taskNr taskInfo taskProperties changeCount origTask curTask cs {TSt|tst & changes = changes}
	// Task and properties unchanged
	| otherwise
		= all_changes taskNr taskInfo taskProperties changeCount origTask curTask cs {TSt|tst & changes = changes}	

all_changes taskNr taskInfo taskProperties changeCount origTask curTask [c:cs] tst=:{TSt|changes}
	= all_changes taskNr taskInfo taskProperties changeCount origTask curTask cs {TSt|tst & changes = [c:changes]}

one_change :: !TaskNr !TaskInfo !TaskProperties !Int !(Task a) !(Task a) !(!ChangeLifeTime, !DynamicId, !Dynamic) ![Maybe (!ChangeLifeTime,!DynamicId,!Dynamic)] !*TSt -> (!TaskResult a,!*TSt) | iTask a
one_change taskNr taskInfo taskProperties changeCount origTask curTask (changeLifeTime, changeId, changeDyn) rest tst
 	# processId = taskNrToString taskNr
 	# (mbProperties, mbTask, mbChange) = appChange changeDyn taskProperties (setTaskContext [changeCount:taskNr] curTask) origTask
	//Determine new change list
	# changes = case mbChange of
			(Just change)	= [Just (changeLifeTime,changeId,change):rest]
			Nothing			= [Nothing:rest]
	//Update task (and properties when changed) 	
	| isJust mbTask
		# changeCount		= inc changeCount 
		# taskProperties	= if (isJust mbProperties) (fromJust mbProperties) taskProperties
		# curTask			= fromJust mbTask					
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties, changeCount = changeCount}) tst
		# tst				= storeTaskFunctionStatic taskNr curTask tst				//Store the changed task with context
		# tst				= storeTaskThread taskNr (createTaskThread curTask) tst
		= do_task taskNr taskInfo taskProperties changeCount curTask {TSt|tst & changes = changes} //Execute
	//Only add properties
	| isJust mbProperties
		# taskProperties	= fromJust mbProperties
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties}) tst
		= do_task taskNr taskInfo taskProperties changeCount curTask {TSt|tst & changes = changes}
	// Task and properties unchanged
	| otherwise
		= do_task taskNr taskInfo taskProperties changeCount curTask {TSt|tst & changes = changes}

do_task :: !TaskNr !TaskInfo !TaskProperties !Int !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a
do_task taskNr taskInfo taskProperties changeCount curTask tst=:{userId,delegatorId, mainTask}
	# tst		= {tst & tree = TTMainTask taskInfo taskProperties []
					, taskNr		= [changeCount:taskNr]
					, mainTask		= taskNrToString taskNr
					, userId		= fst taskProperties.managerProps.worker
					, delegatorId	= fst taskProperties.systemProps.manager
					}
	# (result, tst)	= applyTask curTask tst
	= (result, {TSt | tst & userId = userId, delegatorId = delegatorId, mainTask = mainTask})

//The tricky dynamic part of applying changes
appChange :: !Dynamic !TaskProperties !(Task a) !(Task a) -> (Maybe TaskProperties,Maybe (Task a), Maybe Dynamic) | iTask a
appChange (fun :: A.c: Change c | iTask c) properties curTask origTask
	= fun properties curTask origTask
appChange (fun :: Change a^) properties curTask origTask
	= fun properties curTask origTask
appChange dyn properties curTask origTask
	= (Nothing, Nothing, Just dyn)

setTaskContext :: TaskNr (Task a) -> (Task a)
setTaskContext cxt (Task name _ tf) = Task name (Just cxt) tf

*/


//END OLD ASSIGN IMPLEMENTATION

createTaskThread :: !(Task a) -> (*TSt -> *(!TaskResult Dynamic,!*TSt)) | iTask a
createTaskThread task = createTaskThread` task
where
	createTaskThread` :: !(Task a) !*TSt -> *(!TaskResult Dynamic,!*TSt) | iTask a
	createTaskThread` task tst
		# (result, tst)	= applyTask task tst
		= case result of
			TaskBusy		= (TaskBusy, tst)
			TaskFinished a	= (TaskFinished (dynamic a), tst)
			TaskException e	= (TaskException e, tst)
	

applyChangeToTaskTree :: !ProcessId !Dynamic !ChangeLifeTime !*TSt -> *TSt
applyChangeToTaskTree pid change lifetime tst=:{taskNr,taskInfo,firstRun,userId,delegatorId,tree,mainTask,staticInfo,doChange,changes}
	# (mbProcess,tst) = getProcess pid tst
	= case mbProcess of
		(Just proc) 
			# (_,_,tst) = evaluateTaskInstance proc (Just (lifetime,change)) True tst
			= {tst & taskNr = taskNr, taskInfo = taskInfo, firstRun = firstRun, userId = userId, delegatorId = delegatorId
			  , tree = tree, mainTask = mainTask, staticInfo = staticInfo, doChange = doChange, changes = changes}
		Nothing		
			= tst

getCurrentSession :: !*TSt 	-> (!Session, !*TSt)
getCurrentSession tst =:{staticInfo} = (staticInfo.currentSession, tst)

getCurrentUser :: !*TSt -> (!User, !*TSt)
getCurrentUser tst =: {staticInfo}
	= (staticInfo.currentSession.Session.user, {tst & staticInfo = staticInfo})

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

appWorldTSt	:: !.(*World -> *World) !*TSt -> *TSt
appWorldTSt f tst=:{TSt|world}
	= {TSt|tst & world = f world}

accWorldTSt	:: !.(*World -> *(.a,*World))!*TSt -> (.a,!*TSt)
accWorldTSt f tst=:{TSt|world}
	# (a,world) = f world
	= (a, {TSt|tst & world = world})

mkTaskFunction :: (*TSt -> (!a,!*TSt)) -> (*TSt -> (!TaskResult a,!*TSt))
mkTaskFunction f = \tst -> let (a,tst`) = f tst in (TaskFinished a,tst`)
		
mkInteractiveTask	:: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a 
mkInteractiveTask taskname taskfun = Task {TaskDescription| title = taskname, description = Note ""} Nothing mkInteractiveTask`	
where
	mkInteractiveTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTInteractiveTask taskInfo (abort "No interface definition given") []}

mkInstantTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkInstantTask taskname taskfun = Task {TaskDescription| title = taskname, description = Note ""} Nothing mkInstantTask`
where
	mkInstantTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTFinishedTask taskInfo} //We use a FinishedTask node because the task is finished after one evaluation

mkMonitorTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkMonitorTask taskname taskfun = Task {TaskDescription| title = taskname, description = Note ""} Nothing mkMonitorTask`
where
	mkMonitorTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTMonitorTask taskInfo []}

mkRpcTask :: !String !RPCExecute !(String -> a) -> Task a | gUpdate{|*|} a
mkRpcTask taskname rpce parsefun = Task {TaskDescription| title = taskname, description = Note ""} Nothing mkRpcTask`
where
	mkRpcTask` tst=:{TSt | taskNr, taskInfo}
		# rpce				= {RPCExecute | rpce & taskId = taskNrToString taskNr}
		# (updates, tst) 	= getRpcUpdates tst
		# (rpce, tst) 		= checkRpcStatus rpce tst
		| length updates == 0 
			= (TaskBusy, {tst & tree = TTRpcTask taskInfo rpce })				
		| otherwise 
			= applyRpcUpdates updates tst rpce parsefun
	
	checkRpcStatus :: RPCExecute !*TSt -> (!RPCExecute, !*TSt)
	checkRpcStatus rpce tst 
		# (mbStatus, tst) = getTaskStore "status" tst 
		= case mbStatus of
		Nothing 
			# tst = setTaskStore "status" "Pending" tst
			= ({RPCExecute | rpce & status = "Pending"},tst)
		Just s
			= ({RPCExecute | rpce & status = s},tst)
	
	getRpcUpdates :: !*TSt -> ([(String,String)],!*TSt)
	getRpcUpdates tst=:{taskNr,request} = (updates request, tst)
	where
		updates request
			| http_getValue "_rpctaskid" request.arg_post "" == taskNrToString taskNr
				= [u \\ u =: (k,v) <- request.arg_post]
			| otherwise
				= []

/* Error handling needs to be implemented! */	
applyRpcUpdates :: [(String,String)] !*TSt !RPCExecute !(String -> a) -> *(!TaskResult a,!*TSt) | gUpdate{|*|} a	
applyRpcUpdates [] tst rpce parsefun = (TaskBusy,tst)
applyRpcUpdates [(n,v):xs] tst rpce parsefun
| n == "_rpcresult" 
	# (mbMsg) = fromJSON v
	= case mbMsg of
		Just msg = applyRpcMessage msg tst rpce parsefun
		Nothing  = applyRpcUpdates xs tst rpce parsefun //Ignore the message and go on..
| otherwise 
	= applyRpcUpdates xs tst rpce parsefun
where
	applyRpcMessage msg tst rpci parsfun
		# tst = setStatus msg.RPCMessage.status tst
		| msg.RPCMessage.success
			| msg.RPCMessage.finished 	= (TaskFinished (parsefun msg.RPCMessage.result),tst)
			| otherwise					= (TaskBusy,tst)
		| otherwise
			# (def,tst) = accWorldTSt defaultValue tst
			= (TaskFinished def, tst)
			
	setStatus "" tst		= tst
	setStatus status tst	= setTaskStore "status" status tst
		
mkSequenceTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkSequenceTask taskname taskfun = Task {TaskDescription| title = taskname, description = Note ""} Nothing mkSequenceTask`
where
	mkSequenceTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTSequenceTask taskInfo [], taskNr = [0:taskNr]}
			
mkParallelTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkParallelTask taskname taskfun = Task {TaskDescription| title = taskname, description = Note ""} Nothing mkParallelTask`
where
	mkParallelTask` tst=:{TSt|taskNr,taskInfo}
		# tst = {tst & tree = TTParallelTask taskInfo [], taskNr = [0:taskNr]}												
		= taskfun tst
			
mkMainTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkMainTask taskname taskfun = Task {TaskDescription| title = taskname, description = Note ""} Nothing mkMainTask`
where
	mkMainTask` tst=:{taskNr,taskInfo}
		= taskfun {tst & tree = TTMainTask taskInfo (abort "Executed undefined maintask") []}

applyTask :: !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a
applyTask (Task desc mbCxt taskfun) tst=:{taskNr,tree,dataStore,world}
	# taskId					= iTaskId taskNr ""
	# (taskVal,dataStore,world)	= loadValue taskId dataStore world
	# taskInfo =	{ taskId		= taskNrToString taskNr
					, taskLabel		= desc.TaskDescription.title
					, traceValue	= ""
					}
	# tst = {TSt|tst & dataStore = dataStore, world = world}
	= case taskVal of
		(Just (TaskFinished a))	
			# tst = addTaskNode (TTFinishedTask {taskInfo & traceValue = printToString a}) tst
			= (TaskFinished a, {tst & taskNr = incTaskNr taskNr})

		_
			# tst	= {tst & taskInfo = taskInfo, firstRun = isNothing taskVal }	
			// If the task is new, but has run in a different context, initialize the states of the task and its subtasks
			# tst	= case (taskVal,mbCxt) of
						(Nothing, Just oldTaskNr)	= copyTaskStates oldTaskNr taskNr tst
						_							= tst
			// Execute task function
			# (result, tst)	= taskfun tst
			// Remove user updates (needed for looping. a new task may get the same tasknr again, but should not get the events)
			# tst=:{tree=node,dataStore}	= clearUserUpdates tst
			// Update task state
			= case result of
				(TaskFinished a)
					//Garbage collect
					# tst=:{TSt|dataStore}	= deleteTaskStates taskNr {TSt|tst & dataStore = dataStore}
					
					// Store final value
					# dataStore				= storeValue taskId result dataStore
					# tst					= addTaskNode (TTFinishedTask {taskInfo & traceValue = printToString a}) {tst & taskNr = incTaskNr taskNr, tree = tree, dataStore = dataStore}
					= (TaskFinished a, tst)
				(TaskBusy)
					# tst					= addTaskNode (updateTaskNode node) {tst & taskNr = incTaskNr taskNr, tree = tree, dataStore = dataStore}
					= (TaskBusy, tst)
				(TaskException e)
					# tst					= addTaskNode (updateTaskNode node) {tst & taskNr = incTaskNr taskNr, tree = tree, dataStore = dataStore}
					= (TaskException e, tst)
		
where
	//Increase the task nr
	incTaskNr [] = [0]
	incTaskNr [i:is] = [i+1:is]
	
	//Add a new node to the current sequence or process
	addTaskNode node tst=:{tree} = case tree of
		(TTMainTask ti mti tasks)	= {tst & tree = TTMainTask ti mti [node:tasks]}
		(TTSequenceTask ti tasks)	= {tst & tree = TTSequenceTask ti [node:tasks]}
		(TTParallelTask ti tasks)	= {tst & tree = TTParallelTask ti [node:tasks]}
		_							= {tst & tree = tree}
	
	//update the finished, tasks and traceValue fields of a task tree node
	updateTaskNode (TTInteractiveTask ti defs accA)	= TTInteractiveTask	ti defs accA
	updateTaskNode (TTMonitorTask ti status)		= TTMonitorTask		ti status
	updateTaskNode (TTSequenceTask ti tasks) 		= TTSequenceTask	ti (reverse tasks)
	updateTaskNode (TTParallelTask ti tasks)		= TTParallelTask	ti (reverse tasks)
	updateTaskNode (TTMainTask ti mti tasks)		= TTMainTask		ti mti (reverse tasks)		
	updateTaskNode (TTRpcTask ti rpci)				= TTRpcTask			ti rpci
		
setTUIDef	:: !TUIDef !*TSt -> *TSt
setTUIDef def tst=:{tree}
	= case tree of
		(TTInteractiveTask info _ acts)		= {tst & tree = TTInteractiveTask info (Left def) acts}
		_									= tst

setTUIUpdates :: ![TUIUpdate] !*TSt -> *TSt
setTUIUpdates upd tst=:{tree}
	= case tree of
		(TTInteractiveTask info _ acts)		= {tst & tree = TTInteractiveTask info (Right upd) acts}
		_									= tst
		
setAccActions :: ![(Action,Bool)] !*TSt -> *TSt
setAccActions actions tst=:{tree}
	= case tree of
		(TTInteractiveTask info def _)		= {tst & tree = TTInteractiveTask info def actions}
		_									= tst

setStatus :: ![HtmlTag] !*TSt -> *TSt
setStatus msg tst=:{tree}
	= case tree of
		(TTMonitorTask info _)				= {tst & tree = TTMonitorTask info msg}
		_									= tst

loadTaskFunctionStatic :: !TaskNr !*TSt -> (!Maybe (Task a), !*TSt) | TC a
loadTaskFunctionStatic taskNr tst =: {TSt | dataStore, world}
# (mbDyn, dataStore, world) = loadValue (storekey taskNr) dataStore world
= case mbDyn of 
	(Just (t :: Task a^)) 	= ((Just t), {TSt | tst & dataStore = dataStore, world = world})
	Nothing				  	= (Nothing , {TSt | tst & dataStore = dataStore, world = world})
where
	storekey taskNr  	 = "iTask_"+++(taskNrToString taskNr)+++"-taskfun-static"

storeTaskFunctionStatic :: !TaskNr !(Task a) !*TSt -> *TSt | TC a
storeTaskFunctionStatic taskNr task tst = storeTaskFunction taskNr task "static" tst

storeTaskFunction :: !TaskNr !(Task a) String !*TSt -> *TSt | TC a
storeTaskFunction taskNr task key tst =: {TSt | dataStore}
# dataStore = storeValueAs SFPlain (storekey taskNr key) (dynamic task) dataStore
= {TSt | tst & dataStore = dataStore}
where
	storekey taskNr key = "iTask_"+++(taskNrToString taskNr)+++"-taskfun-"+++key 

storeTaskThread :: !TaskNr !(*TSt -> *(!TaskResult Dynamic,!*TSt)) !*TSt -> *TSt
storeTaskThread taskNr thread tst =:{dataStore}
	# dataStore = storeValueAs SFDynamic key (dynamic thread :: *TSt -> *(!TaskResult Dynamic,!*TSt)) dataStore
	= {TSt | tst & dataStore = dataStore}
where
	key = "iTask_" +++ (taskNrToString taskNr) +++ "-thread"

loadTaskThread :: !TaskNr !*TSt -> (*TSt -> *(!TaskResult Dynamic,!*TSt), !*TSt)
loadTaskThread taskNr tst =:{dataStore,world}
	# (mbDyn, dataStore, world)	= loadValue key dataStore world
	= case mbDyn of
		(Just (f :: *TSt -> *(!TaskResult Dynamic,!*TSt)))
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
loadProcessResult :: !TaskNr !*TSt -> (!Maybe (TaskResult Dynamic), !*TSt)
loadProcessResult taskNr tst =:{dataStore, world}
	# (mbDyn, dataStore, world) = loadValue key dataStore world
	= case mbDyn of
		( Just (result :: TaskResult Dynamic))	= (Just result, {TSt | tst & dataStore = dataStore, world = world})
		_										= (Nothing, {TSt | tst & dataStore = dataStore, world = world})
where
	key = "iTask_"+++(taskNrToString taskNr)+++"-result"
	
storeProcessResult :: !TaskNr !(TaskResult Dynamic) !*TSt -> *TSt
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
		
resetSequence :: !*TSt -> *TSt
resetSequence tst=:{taskNr,tree}
	= case tree of
		(TTSequenceTask info sequence)	= {tst & taskNr = [0:tl taskNr], tree = TTSequenceTask info []}
		_								= {tst & tree = tree}

deleteTaskStates :: !TaskNr !*TSt -> *TSt
deleteTaskStates taskNr tst=:{TSt|dataStore,world}
	# (dataStore,world) = deleteValues (iTaskId taskNr "") dataStore world
	= {TSt|tst & world = world, dataStore = dataStore} 
	
copyTaskStates :: !TaskNr !TaskNr !*TSt	-> *TSt
copyTaskStates fromtask totask tst=:{TSt|dataStore,world}
	# (dstore,world) = copyValues (iTaskId fromtask "") (iTaskId totask "") dataStore world
	= {TSt|tst & dataStore = dstore, world = world}

flushStore :: !*TSt -> *TSt
flushStore tst=:{TSt|dataStore,systemStore,documentStore,world}
	# (dstore,world) = flushCache dataStore world
	# (sstore,world) = flushCache systemStore world
	# (fstore,world) = flushCache documentStore world
	= {TSt|tst & dataStore = dstore, systemStore = sstore, documentStore = fstore, world = world}

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
taskLabel (Task desc _ _) = desc.TaskDescription.title