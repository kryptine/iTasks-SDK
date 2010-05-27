implementation module TSt

import StdEnv, StdMaybe
import Http, Util, Text
import ProcessDB, SessionDB, ChangeDB, DocumentDB, UserDB, TaskTree
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
derive JSONDecode 	RPCMessage

mkTSt :: String Config HTTPRequest Session ![Workflow] !*Store !*Store !*World -> *TSt
mkTSt appName config request session workflows dataStore documentStore world
	=	{ taskNr		= []
		, taskInfo		= initTaskInfo
		, tree			= TTMainTask initTaskInfo initTaskProperties Nothing Nothing (TTFinishedTask initTaskInfo [])
		, newTask		= True
		, mainTask		= ""
		, properties	= initTaskProperties
		, menus			= Nothing
		, menusChanged	= False
		, staticInfo	= initStaticInfo appName session workflows
		, currentChange	= Nothing
		, pendingChanges= []
		, config		= config
		, request		= request
		, dataStore		= dataStore
		, documentStore	= documentStore
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
		, worker = UserName "" ""
		, groupedBehaviour = GBFixed
		, taskDescription = ""
		}

initSystemProperties :: SystemProperties
initSystemProperties =
	{SystemProperties
	| processId = ""
	, manager = UserName "" ""
	, issuedAt = Timestamp 0
	, firstEvent = Nothing
	, latestEvent = Nothing
	, latestExtEvent = Nothing
	, subTaskWorkers = []
	, deleteWhenDone = False
	}
	
initManagerProperties :: ManagerProperties
initManagerProperties = 
	{ManagerProperties
	| worker = UserName "" ""
	, subject = ""
	, priority = NormalPriority
	, deadline = Nothing
	}

initWorkerProperties :: WorkerProperties
initWorkerProperties =
	{WorkerProperties
	| progress = TPActive
	}
	
initTaskProperties :: TaskProperties
initTaskProperties
	= { systemProps		= initSystemProperties
	  , managerProps	= initManagerProperties
	  , workerProps		= initWorkerProperties
	  }
		  
createTaskInstance :: !Dynamic !Bool !(Maybe TaskParallelType) !Bool !Bool !*TSt -> (!Dynamic,!ProcessId,!*TSt)
createTaskInstance thread=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) toplevel mbParType activate delete tst=:{taskNr,mainTask}
	//-> the current assigned worker is also the manager of all the tasks IN the process (excluding the main task)
	# (worker,tst)			= getCurrentWorker tst
	# (manager,tst) 		= if (worker <> unknownUser) (worker,tst) (getCurrentUser tst)	
	# (currentTime, tst)	= accWorldTSt time tst
	# processId				= if toplevel "" (taskNrToString taskNr)
	# parent				= if toplevel "" mainTask
	# managerProps			= setUser manager (taskProperties originalTask)
	# properties =
		{TaskProperties
		| systemProps =
			{SystemProperties
			| processId	= ""
			, manager		= toUserName manager
			, issuedAt		= currentTime
			, firstEvent	= Nothing
			, latestEvent	= Nothing
			, latestExtEvent = Nothing
			, subTaskWorkers = []
			, deleteWhenDone = delete
			}
		, managerProps = managerProps
		, workerProps =
			{WorkerProperties
			| progress	= TPActive
			}
		}
	# process =
		{ Process
		| processId		 = processId
		, status		 = if activate Active Suspended
		, parent		 = parent
		, properties	 = properties
		, changeCount	 = 0
		, mutable		 = True
		, menus			 = Nothing
		, inParallelType = mbParType
		}
	//Create an entry in the process table
	# (processId, tst)		= createProcess process tst
	//Load process as it is in the store
	# (mbProcess,tst)		= getProcess processId tst
	# process				= fromJust mbProcess
	//Create a thread with task functions in the store
	# tst					= storeThread processId thread tst
	| activate
		//If directly active, evaluate the process once to kickstart automated steps that can be set in motion immediately	
		# (result,tree,tst)		= evaluateTaskInstance process Nothing toplevel True {tst & staticInfo = {tst.staticInfo & currentProcessId = processId}}
		= case result of
			TaskBusy				= (dynamic TaskBusy :: TaskResult a, processId, tst)
			TaskFinished (r :: a)	= (dynamic TaskFinished r :: TaskResult a, processId, tst)
			TaskException e			= (dynamic TaskException e :: TaskResult a, processId, tst)
	| otherwise
		= (dynamic TaskBusy :: TaskResult a, processId, tst)
where
	setUser manager props=:{ManagerProperties|worker=UserName "" ""} = {ManagerProperties|props & worker = toUserName manager}
	setUser manager props = props


//NEW THREAD FUNCTIONS
createThread :: !(Task a) -> Dynamic	| iTask a
createThread task = (dynamic container :: Container (TaskThread a^) a^)
where
 	container = Container {TaskThread|originalTask = task, currentTask = task}

applyThread :: !Dynamic *TSt -> (!TaskResult Dynamic, !*TSt)
applyThread (Container {TaskThread|currentTask} :: Container (TaskThread a) a) tst
	# (result, tst) = applyTask currentTask tst
	= case result of
			TaskBusy		= (TaskBusy, tst)
			TaskFinished a	= (TaskFinished (dynamic a), tst)
			TaskException e	= (TaskException e, tst)

storeThread :: !ProcessId !Dynamic !*TSt -> *TSt
storeThread processId thread tst=:{TSt|dataStore}
	# dataStore = storeValueAs SFDynamic ("iTask_" +++ processId +++ "-thread") thread dataStore
	= {tst & dataStore = dataStore}

loadThread :: !ProcessId !*TSt -> (!Dynamic,!*TSt)
loadThread processId tst=:{TSt|dataStore,world}
	# (mbThread, dataStore, world)	= loadValue ("iTask_" +++ processId +++ "-thread") dataStore world
	= case mbThread of
		Just thread	= (thread,{tst & dataStore = dataStore, world = world})
		Nothing		= abort "Could not load task thread"
		
//END NEW THREAD FUNCTIONS

//Computes a workflow (sub) process
evaluateTaskInstance :: !Process !(Maybe ChangeInjection) !Bool !Bool !*TSt-> (!TaskResult Dynamic, !TaskTree, !*TSt)
evaluateTaskInstance process=:{Process | processId, parent, properties, menus, changeCount, inParallelType} newChange isTop firstRun tst=:{TSt|currentChange,pendingChanges,mainTask,properties=parentProperties,menus=parentMenus}
	// Reset the task state
	# tst								= resetTSt processId properties inParallelType tst
	// Queue all stored persistent changes (only when run as top node)
	# tst								= if isTop (loadPersistentChanges processId tst) tst
	// When a change is injected set it as active change
	# tst								= if (isJust newChange) {tst & currentChange = newChange} tst
	// Load the task instance
	# (thread,tst)						= loadThread processId tst
	// On the first run, apply all changes. Pending persistent ones as well as the currently active change
	// On subsequent runs only apply the currently active change
	// After each application of a change the thread is evaluated because the full evaluated tree must be the
	// subject of later changes
	# (changeCount,thread,properties,menus,result,tst)
										= if firstRun
											(applyAllChanges processId changeCount pendingChanges thread properties menus tst)
											(applyCurrentChange processId changeCount thread properties menus tst)
	// The tasktree of this process is the tree as it has been constructed, but with updated properties
	# (TTMainTask ti _ _ _ tasks,tst)	= getTaskTree tst
	# tree								= TTMainTask ti properties menus inParallelType tasks
	// Store the adapted persistent changes
	# tst								= if isTop (storePersistentChanges processId tst) tst
	# tst								= restoreTSt mainTask parentProperties parentMenus tst
	= case result of
		TaskBusy
			//Update process table (changeCount & properties)
			# (_,tst)	= updateProcess processId (\p -> {Process|p & properties = properties, menus = menus, changeCount = changeCount }) tst
			= (TaskBusy, tree, tst)
		TaskFinished dyn
			//Store result
			# tst 		= storeProcessResult (taskNrFromString processId) result tst
			//Update process table (status, changeCount & properties)
			# (_,tst)	= updateProcess processId (\p -> {Process|p & status = Finished, properties = properties, menus = menus, changeCount = changeCount }) tst
			| isTop
				//Evaluate parent process
				| parent <> ""
					# (mbParentProcess,tst) = getProcess parent tst
					= case mbParentProcess of
						Nothing 
							= (result,tree,tst)
						Just parentProcess
							# (_,_,tst)	= evaluateTaskInstance parentProcess Nothing True False tst
							= (result,tree,tst)	
				| otherwise
					= (result,tree,tst)
			| otherwise
				//Update process table (changeCount & properties)
				# (_,tst)	= updateProcess processId (\p -> {Process|p & properties = properties, menus = menus, changeCount = changeCount }) tst
				= (result,tree,tst)
		TaskException e
			//Store exception
			# tst 		= storeProcessResult (taskNrFromString processId) result tst
			//Update process table
			# (_,tst)	= updateProcess processId (\p -> {Process|p & status = Excepted}) tst
			= (TaskException e, tree, tst)
where
	resetTSt :: !ProcessId !TaskProperties !(Maybe TaskParallelType) !*TSt -> *TSt
	resetTSt processId properties inptype tst
		# taskNr	= taskNrFromString processId
		# info		= {TaskInfo|taskId = toString processId, taskLabel = properties.managerProps.subject, traceValue = "", worker=properties.managerProps.ManagerProperties.worker, groupedBehaviour = GBFixed, taskDescription = ""}
		# tree		= TTMainTask info properties menus inptype (TTFinishedTask info [])
		= {TSt| tst & taskNr = taskNr, tree = tree, staticInfo = {tst.staticInfo & currentProcessId = processId}, mainTask = processId}
	
	
	restoreTSt :: !ProcessId !TaskProperties !(Maybe [Menu]) !*TSt -> *TSt
	restoreTSt mainTask properties menus tst = {TSt|tst & mainTask = mainTask, properties = properties, menus = menus}
	/*
	* Load all stored persistent changes that are applicable to the current (sub) process.
	* In case of evaluating a subprocess, this also includes the changes that have been injected
	* at parent nodes. Persistent changes are queued in the temporal order in which they were injected.
	* The oldest change is at the head of the list.
	*/
	loadPersistentChanges :: !ProcessId !*TSt -> *TSt 
	loadPersistentChanges processId tst
		# (changes,tst)	= getChangesForProcess processId tst
		# (changes,tst) = loadChangeFunctions changes tst
		= {tst & pendingChanges = changes}
	where
		loadChangeFunctions [] tst = ([],tst)
		loadChangeFunctions [{PersistentChange|label}:cs] tst=:{TSt|dataStore,world}
			# (mbDyn, dataStore, world)	= loadValue ("iTask_change-" +++ label) dataStore world
			# (dyns,tst)	= loadChangeFunctions cs {TSt|tst & dataStore = dataStore, world = world}
			= case mbDyn of
				Nothing		= (dyns,tst)
				Just dyn	= ([(CLPersistent label,dyn):dyns],tst)
	/*
	* When a (sub)process is evaluated for the first time, all existing persistent changes need to be applied.
	* Because applying a change may result in the creation of new sub processes, changes must be carefully applied
	* in the right order. 
	*/
	applyAllChanges :: !ProcessId !Int [(!ChangeLifeTime,!Dynamic)] !Dynamic !TaskProperties !(Maybe [Menu]) !*TSt -> (!Int, !Dynamic, !TaskProperties, !Maybe [Menu], !TaskResult Dynamic, !*TSt)
	applyAllChanges processId changeCount [] thread properties menus tst
		//Only apply the current change
		= applyCurrentChange processId changeCount thread properties menus tst
	applyAllChanges processId changeCount changes thread properties menus tst=:{currentChange}
		//Add the pending changes one after another (start empty)
		= applyAllChanges` processId changeCount currentChange changes thread properties menus {tst & pendingChanges = []} 
	where
		applyAllChanges` processId changeCount currentChange [] thread properties menus tst
			= applyCurrentChange processId changeCount thread properties menus {tst & currentChange = currentChange}
		applyAllChanges` processId changeCount currentChange [c:cs] thread properties menus tst
			# (changeCount,thread,properties,menus,result,tst) = applyCurrentChange processId changeCount thread properties menus {tst & currentChange = Just c} 
			//Update pending changes list
			# tst	= {tst & pendingChanges = (case tst.currentChange of
														Nothing = tst.pendingChanges
														Just change = tst.pendingChanges ++ [change]) }	
			= case result of
				TaskBusy
					//Continue applying changes
					= applyAllChanges` processId changeCount currentChange cs thread properties menus tst
				TaskFinished val
					//A change caused the task to complete. Stop, but keep pending changes
					= (changeCount,thread,properties,menus,result,{tst & pendingChanges = tst.pendingChanges ++ cs, currentChange = currentChange})
				TaskException e
					//A change caused an exception. Stop, but keep pending changes
					= (changeCount,thread,properties,menus,result,{tst & pendingChanges = tst.pendingChanges ++ cs, currentChange = currentChange})

	applyCurrentChange :: !ProcessId !Int !Dynamic !TaskProperties !(Maybe [Menu]) !*TSt -> (!Int, !Dynamic, !TaskProperties, !(Maybe [Menu]), !TaskResult Dynamic, !*TSt)
	applyCurrentChange processId changeCount thread properties menus tst=:{currentChange}
		= case currentChange of
			Just (lifetime,change)
				// Apply the active change
				# (mbChange,mbThread,properties)	= applyChange [changeCount: taskNrFromString processId] change thread properties
				// Determine the thread to run (the original or the changed one)
				# (changeCount,thread,tst)	= case mbThread of
												Nothing
													= (changeCount,thread,tst)
												Just newThread
													// Store the thread when changed
													# tst = storeThread processId newThread tst
													// Mark all subprocesses of the current thread as immutable
													// They are only kept for the purpose of being copied when the new thread executes
													# tst = setImmutable (taskNrToString [changeCount: taskNrFromString processId]) tst
													// IMPORTANT: change count is incremented if a change produces a new task
													= (inc changeCount,newThread,tst)
				// Update the current change in the task state or remove from the store
				# tst = case mbChange of
									Nothing
										//When a persistent change does not return a new change it
										//is removed from the store
										= case lifetime of
											CLPersistent label
												# tst=:{dataStore,world}	= deleteChange label tst
												# (dataStore,world)			= deleteValues ("iTask_change-" +++ label) dataStore world 
												= {tst & dataStore = dataStore, world = world, currentChange = Nothing}
											_
												= {tst & currentChange = Nothing}
									Just change
										= {tst & currentChange = Just (lifetime,change)}
				// Evaluate the thread
				// IMPORTANT: The taskNr is reset with the latest change count
				# (result,tst=:{TSt|properties,menus})
					= applyThread thread {TSt|tst & taskNr = [changeCount: taskNrFromString processId], properties = properties, menus = menus}
				= (changeCount,thread,properties,menus,result,{TSt|tst & properties = properties, menus = menus})
			Nothing
				# (result,tst=:{TSt|properties,menus})
					= applyThread thread {TSt|tst & taskNr = [changeCount: taskNrFromString processId], properties = properties, menus = menus}
				= (changeCount,thread,properties,menus,result,{TSt|tst & properties = properties, menus = menus})
	
	applyChange :: !TaskNr !Dynamic !Dynamic !TaskProperties -> (!Maybe Dynamic, !Maybe Dynamic, !TaskProperties)
	//Apply a change that matches a specific type
	applyChange cxt (changeFun :: Change a) (Container (thread=:{TaskThread|originalTask,currentTask}) :: Container (TaskThread a) a) properties
		# (mbProps,mbTask,mbChange) = changeFun properties (setTaskContext cxt currentTask) originalTask
		# newThread		= case mbTask of
								Just task	= Just (dynamic Container {thread & currentTask = task} :: Container (TaskThread a) a)
								Nothing		= Nothing
		# newProperties = case mbProps of
								Just props	= props
								Nothing		= properties
		= (mbChange,newThread,newProperties)
	//Apply a change that matches any type
	applyChange cxt (changeFun :: A.c: Change c | iTask c) (Container (thread=:{TaskThread|originalTask,currentTask}) :: Container (TaskThread a) a) properties
		# (mbProps,mbTask,mbChange) = changeFun properties (setTaskContext cxt currentTask) originalTask
		# newThread		= case mbTask of
								Just task	= Just (dynamic Container {thread & currentTask = task} :: Container (TaskThread a) a)
								Nothing		= Nothing
		# newProperties = case mbProps of
								Just props	= props
								Nothing		= properties
		= (mbChange,newThread,newProperties)
	//Change did not match, do nothing
	applyChange cxt change thread properties
		= (Just change, Nothing, properties)
		
	/*
	* Adds the task number at which it has run before to a task
	*/
	setTaskContext :: !TaskNr !(Task a) -> (Task a)
	setTaskContext cxt (Task props gb _ tf) = Task props gb (Just cxt) tf
	
	/*
	* Store the changes that are still active after  a run.
	* Transient changes are discarded.
	*/
	storePersistentChanges :: !ProcessId !*TSt -> *TSt
	storePersistentChanges processId tst=:{currentChange,pendingChanges}
		//Update pending changes
		# tst = storeChanges [(label,dyn) \\ (CLPersistent label,dyn) <- pendingChanges] tst
		//Add new persistent change
		= case currentChange of
					Just (CLPersistent label, dyn)
						# tst = createChange {PersistentChange|label=label,scope=processId} tst
						# tst = storeChanges [(label,dyn)]tst
						= tst 
					_
						= tst 
	where
		storeChanges [] tst = tst
		storeChanges [(label,dyn):cs] tst=:{dataStore} 
			= storeChanges cs {tst & dataStore = (storeValueAs SFDynamic ("iTask_change-" +++ label) dyn dataStore)} 
			
applyChangeToTaskTree :: !ProcessId !ChangeInjection !*TSt -> *TSt
applyChangeToTaskTree pid (lifetime,change) tst=:{taskNr,taskInfo,tree,mainTask,staticInfo,currentChange,pendingChanges, properties, menus}
	# (mbProcess,tst) = getProcess pid tst
	= case mbProcess of
		(Just proc) 
			# (_,_,tst) = evaluateTaskInstance proc (Just (lifetime,change)) True False tst
			= {tst & taskNr = taskNr, taskInfo = taskInfo,properties = properties, menus = menus
			  , tree = tree, mainTask = mainTask, staticInfo = staticInfo, currentChange = currentChange, pendingChanges = pendingChanges}
		Nothing		
			= tst

calculateTaskTree :: !ProcessId !*TSt -> (!TaskTree, !*TSt)
calculateTaskTree processId tst
	# (mbProcess,tst) = getProcess processId tst
	= case mbProcess of
		Nothing
			= (TTFinishedTask {TaskInfo|taskId = toString processId, taskLabel = "Deleted Process", traceValue="Deleted", worker = UserName "" "", groupedBehaviour = GBFixed, taskDescription="Task Result"} [], tst)
		Just process=:{Process|status,properties}
			= case status of
				Active
					//Evaluate the process
					# (result,tree,tst) = evaluateTaskInstance process Nothing True False tst
					= (tree,tst)
				_		
					//retrieve process result from store and show it??
					= (TTFinishedTask {TaskInfo|taskId = toString processId, taskLabel = properties.managerProps.subject, traceValue = "Finished", worker = properties.managerProps.ManagerProperties.worker, groupedBehaviour = GBFixed, taskDescription="Task Result"} [], tst)

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

getCurrentSession :: !*TSt 	-> (!Session, !*TSt)
getCurrentSession tst =:{staticInfo} = (staticInfo.currentSession, tst)

getCurrentUser :: !*TSt -> (!User, !*TSt)
getCurrentUser tst =:{staticInfo}
	= (staticInfo.currentSession.user, {TSt | tst & staticInfo = staticInfo})

getCurrentProcess :: !*TSt -> (!ProcessId, !*TSt)
getCurrentProcess tst =: {staticInfo}
	= (staticInfo.currentProcessId, {tst & staticInfo = staticInfo})

getCurrentWorker :: !*TSt -> (!User, !*TSt)
getCurrentWorker tst =: {TSt | properties}
	= getUser (properties.managerProps.ManagerProperties.worker) {TSt | tst & properties = properties}

getTaskTree :: !*TSt	-> (!TaskTree, !*TSt)
getTaskTree tst =: {tree}
	= (tree, {tst & tree = tree})

getWorkflows :: !*TSt -> (![Workflow],!*TSt)
getWorkflows tst=:{staticInfo = staticInfo =:{staticWorkflows}}
	= (staticWorkflows, {tst & staticInfo = {staticInfo & staticWorkflows = staticWorkflows}})

getWorkflowByName :: !String !*TSt -> (!Maybe Workflow, !*TSt)
getWorkflowByName name tst
	# (workflows, tst)	= getWorkflows tst
	= case filter (\wf -> wf.Workflow.path == name) workflows of
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
mkInteractiveTask taskname taskfun = Task {initManagerProperties & subject = taskname} GBFixed Nothing mkInteractiveTask`	
where
	mkInteractiveTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTInteractiveTask taskInfo (abort "No interface definition given")}

mkInstantTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkInstantTask taskname taskfun = Task {initManagerProperties & subject = taskname} GBFixed Nothing mkInstantTask`
where
	mkInstantTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTFinishedTask taskInfo []} //We use a FinishedTask node because the task is finished after one evaluation

mkMonitorTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkMonitorTask taskname taskfun = Task {initManagerProperties & subject = taskname} GBFixed Nothing mkMonitorTask`
where
	mkMonitorTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTMonitorTask taskInfo []}

mkInstructionTask :: !String !(*TSt -> *(!TaskResult Void,!*TSt)) -> Task Void
mkInstructionTask taskname taskfun = Task {initManagerProperties & subject = taskname} GBFixed Nothing mkInstructionTask`
where
	mkInstructionTask` tst =:{TSt | taskInfo}
		= taskfun {tst & tree = TTInstructionTask taskInfo [] Nothing}

mkRpcTask :: !String !RPCExecute !(String -> a) -> Task a | gUpdate{|*|} a
mkRpcTask taskname rpce parsefun = Task {initManagerProperties & subject = taskname} GBFixed Nothing mkRpcTask`
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

mkExtProcessTask :: !String !String !(*TSt -> *(!TaskResult Int,!*TSt)) -> Task Int
mkExtProcessTask taskname cmdline taskfun = Task {initManagerProperties & subject = taskname} GBFixed Nothing mkExtProcessTask`
where
	mkExtProcessTask` tst =:{TSt | taskInfo}
		= taskfun {tst & tree = TTExtProcessTask taskInfo cmdline}
		
mkSequenceTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkSequenceTask taskname taskfun = Task {initManagerProperties & subject = taskname} GBFixed Nothing mkSequenceTask`
where
	mkSequenceTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTSequenceTask taskInfo [], taskNr = [0:taskNr]}
			
mkParallelTask :: !String !TaskParallelInfo !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkParallelTask taskname tpi taskfun = Task {initManagerProperties & subject = taskname} GBFixed Nothing mkParallelTask`
where
	mkParallelTask` tst=:{TSt|taskNr,taskInfo}
		# tst = {tst & tree = TTParallelTask taskInfo tpi [], taskNr = [0:taskNr]}												
		= taskfun tst

mkGroupedTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkGroupedTask taskname taskfun = Task {initManagerProperties & subject = taskname} GBFixed Nothing mkGroupedTask`
where
	mkGroupedTask` tst=:{TSt|taskNr,taskInfo}
		# tst = {tst & tree = TTGroupedTask taskInfo [] [], taskNr = [0:taskNr]}
		= taskfun tst
			
mkMainTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkMainTask taskname taskfun = Task {initManagerProperties & subject = taskname} GBFixed Nothing mkMainTask`
where
	mkMainTask` tst=:{taskNr,taskInfo}
		= taskfun {tst & tree = TTMainTask taskInfo initTaskProperties Nothing Nothing (TTFinishedTask taskInfo [])}

applyTask :: !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a
applyTask (Task initProperties groupedBehaviour mbInitTaskNr taskfun) tst=:{taskNr,tree,dataStore,world,properties}
	# taskId					= iTaskId taskNr ""
	# (taskVal,dataStore,world)	= loadValue taskId dataStore world
	# taskInfo =	{ taskId			= taskNrToString taskNr
					, taskLabel			= initProperties.subject
					, traceValue		= ""
					, worker			= properties.managerProps.ManagerProperties.worker
					, groupedBehaviour 	= groupedBehaviour
					, taskDescription	= ""
					}
	# tst = {TSt|tst & dataStore = dataStore, world = world, taskInfo = taskInfo, newTask = isNothing taskVal}
	= case taskVal of
		(Just (TaskFinished a))	
			# tst = addTaskNode (TTFinishedTask {taskInfo & traceValue = printToString a} (visualizeAsHtmlDisplay a)) tst
			= (TaskFinished a, {tst & taskNr = incTaskNr taskNr})
		_
			// If the task is new, but has run in a different context, initialize the states of the task and its subtasks
			# tst	= case (taskVal,mbInitTaskNr) of
						(Nothing, Just initTaskNr)	= copyTaskStates initTaskNr taskNr tst
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
					# tst					= addTaskNode (TTFinishedTask {taskInfo & traceValue = printToString a} (visualizeAsHtmlDisplay a)) {tst & taskNr = incTaskNr taskNr, tree = tree, dataStore = dataStore}
					= (TaskFinished a, tst)
				(TaskBusy)
					// Store intermediate value
					# dataStore				= storeValue taskId result dataStore
					# tst					= addTaskNode (finalizeTaskNode node) {tst & taskNr = incTaskNr taskNr, tree = tree, dataStore = dataStore}
					= (TaskBusy, tst)
				(TaskException e)
					// Store exception
					# dataStore				= storeValue taskId result dataStore
					# tst					= addTaskNode (TTFinishedTask {taskInfo & traceValue = "Exception"} [Text "Uncaught exception"]) {tst & taskNr = incTaskNr taskNr, tree = tree, dataStore = dataStore}
					= (TaskException e, tst)
		
where
	//Increase the task nr
	incTaskNr [] = [0]
	incTaskNr [i:is] = [i+1:is]
	
	//Add a new node to the current sequence or process
	addTaskNode node tst=:{tree} = case tree of
		(TTMainTask ti mti menus inptype task)	= {tst & tree = TTMainTask ti mti menus inptype node} 	//Just replace the subtree 
		(TTSequenceTask ti tasks)				= {tst & tree = TTSequenceTask ti [node:tasks]}			//Add the node to the sequence
		(TTParallelTask ti tpi tasks)			= {tst & tree = TTParallelTask ti tpi [node:tasks]}		//Add the node to the parallel set
		(TTGroupedTask ti tasks gActions)		= {tst & tree = TTGroupedTask ti [node:tasks] gActions}	//Add the node to the grouped set
		_										= {tst & tree = tree}
	
	//Perform reversal of lists that have been accumulated in reversed order
	finalizeTaskNode (TTSequenceTask ti tasks) 			= TTSequenceTask	ti (reverse tasks)
	finalizeTaskNode (TTParallelTask ti tpi tasks)		= TTParallelTask	ti tpi (reverse tasks)
	finalizeTaskNode (TTGroupedTask ti tasks gActions)	= TTGroupedTask		ti (reverse tasks) gActions
	finalizeTaskNode node								= node
	
setTUIDef	:: !(TUIDef,[TUIButton]) [HtmlTag] ![(Action,Bool)] ![(!Action, !Hotkey)] !*TSt -> *TSt
setTUIDef def taskDescription accActions hotkeys tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask {info & taskDescription = foldl (+++) "" (map toString taskDescription)} (Definition def accActions hotkeys)}
		_								= tst

setTUIUpdates :: ![TUIUpdate] ![(Action,Bool)] ![(!Action, !Hotkey)] !*TSt -> *TSt
setTUIUpdates upd accActions hotkeys tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask info (Updates upd accActions hotkeys)}
		_								= tst
		
setTUIFunc :: (*TSt -> *(!InteractiveTask, !*TSt)) [HtmlTag] !*TSt -> *TSt
setTUIFunc func taskDescription tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask {info & taskDescription = foldl (+++) "" (map toString taskDescription)} (Func func)}
		_								= tst

setTUIMessage :: !(TUIDef,[TUIButton]) [HtmlTag] ![(Action,Bool)] ![(!Action, !Hotkey)] !*TSt -> *TSt
setTUIMessage msg taskDescription accActions hotkeys tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask {info & taskDescription = foldl (+++) "" (map toString taskDescription)} (Message msg accActions hotkeys)}
		_								= tst

setStatus :: ![HtmlTag] !*TSt -> *TSt
setStatus msg tst=:{tree}
	= case tree of
		(TTMonitorTask info _)				= {tst & tree = TTMonitorTask info msg}
		_									= tst
		
setGroupActions :: ![(Action, (Either Bool (*TSt -> *(!Bool,!*TSt))))] !*TSt -> *TSt
setGroupActions actions tst=:{tree}
	= case tree of
		(TTGroupedTask info tasks _)	= {tst & tree = TTGroupedTask info tasks actions}
		_								= tst

/**
* Store and load the result of a workflow instance
*/
loadProcessResult :: !TaskNr !*TSt -> (!Maybe (TaskResult Dynamic), !*TSt)
loadProcessResult taskNr tst =:{dataStore, world}
	# (mbResult, dataStore, world) = loadValue key dataStore world
	= (mbResult, {TSt | tst & dataStore = dataStore, world = world})
where
	key = "iTask_"+++(taskNrToString taskNr)+++"-result"
	
storeProcessResult :: !TaskNr !(TaskResult Dynamic) !*TSt -> *TSt
storeProcessResult taskNr result tst=:{dataStore}
	# dataStore	= storeValueAs SFDynamic key result dataStore
	= {TSt |tst & dataStore = dataStore}
where
	key = "iTask_"+++(taskNrToString taskNr)+++"-result"
	
setTaskStore :: !String !a !*TSt -> *TSt | iTask a
setTaskStore key value tst=:{taskNr}
	= setTaskStoreFor taskNr key value tst
	
setTaskStoreFor :: !TaskNr !String !a !*TSt -> *TSt | iTask a
setTaskStoreFor taskNr key value tst=:{dataStore}
	# dataStore = storeValue storekey value dataStore
	= {TSt|tst & dataStore = dataStore}
where
	storekey = "iTask_" +++ (taskNrToString taskNr) +++ "-" +++ key

getTaskStore :: !String !*TSt -> (Maybe a, !*TSt) | iTask a
getTaskStore key tst=:{taskNr}
	= getTaskStoreFor taskNr key tst

getTaskStoreFor	:: !TaskNr !String !*TSt -> (Maybe a, !*TSt) | iTask a
getTaskStoreFor taskNr key tst=:{dataStore,world}
	# (mbValue,dataStore,world) = loadValue storekey dataStore world
	= (mbValue,{TSt|tst&dataStore = dataStore, world = world})
where
	storekey = "iTask_" +++ (taskNrToString taskNr) +++ "-" +++ key

getUserUpdates :: !*TSt -> ([(String,String)],!*TSt)
getUserUpdates tst=:{taskNr,request} = (updates request, tst)
where
	updates request
		| http_getValue "_targettask" request.arg_post "" == taskNrToString taskNr
			= [u \\ u =:(k,v) <- request.arg_post | k.[0] <> '_']
		| otherwise
			= []

getChildrenUpdatesFor :: !TaskNr !*TSt -> ([(String,String)],!*TSt)
getChildrenUpdatesFor taskNr tst=:{request} = (updates request, tst);
where
	updates request
		| startsWith (taskNrToString taskNr) (http_getValue "_targettask" request.arg_post "")
			= [u \\ u =:(k,v) <- request.arg_post]
		| otherwise
			= []
			
anyUpdates :: !*TSt -> (Bool,!*TSt)
anyUpdates tst=:{request} = (http_getValue "_targettask" request.arg_post "" <> "",tst)
			
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
	// Delete values in the data store
	# (dataStore,world) = deleteValues (iTaskId taskNr "") dataStore world
	// Delete subprocesses in the process table
	# tst				= deleteSubProcesses (taskNrToString taskNr) {TSt|tst & world = world, dataStore = dataStore} 
	= tst
	
copyTaskStates :: !TaskNr !TaskNr !*TSt	-> *TSt
copyTaskStates fromtask totask tst=:{TSt|dataStore,world}
	// Copy values in the data store
	# (dstore,world) 	= copyValues (iTaskId fromtask "") (iTaskId totask "") dataStore world
	// Copy subprocess in the process table
	# tst				= copySubProcesses (taskNrToString fromtask) (taskNrToString totask) {TSt|tst & dataStore = dstore, world = world}
	= tst
	
flushStore :: !*TSt -> *TSt
flushStore tst=:{TSt|dataStore,documentStore,world}
	# (dataStore,world) = flushCache dataStore world
	# (documentStore,world) = flushCache documentStore world
	= {TSt|tst & dataStore = dataStore, documentStore = documentStore, world = world}

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
taskLabel (Task p _ _ _) = p.subject

taskProperties :: !(Task a) -> ManagerProperties
taskProperties (Task p _ _ _) = p
