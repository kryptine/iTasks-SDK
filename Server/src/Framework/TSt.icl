implementation module TSt

import StdEnv, StdMaybe
import Http, Util, Text
import ProcessDB, SessionDB, ChangeDB, DocumentDB, UserDB, TaskTree
import CommonDomain

import GenEq, GenVisualize, GenUpdate, Store, Config

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

derive bimap 		Maybe, (,)
derive JSONDecode 	RPCMessage

mkTSt :: String Config HTTPRequest ![Workflow] !*Store !*World -> *TSt
mkTSt appName config request workflows store world
	=	{ taskNr		= []
		, taskInfo		= initTaskInfo
		, tree			= TTMainTask initTaskInfo initTaskProperties Nothing Nothing (TTFinishedTask initTaskInfo NoOutput)
		, treeType		= SpineTree
		, newTask		= True
		, events		= []
		, properties	= initTaskProperties
		, menus			= Nothing
		, menusChanged	= False
		, staticInfo	= initStaticInfo appName workflows
		, currentChange	= Nothing
		, pendingChanges= []
		, request		= request
		, iworld		= initIWorld appName config store world
		}

initStaticInfo :: String ![Workflow] -> StaticInfo
initStaticInfo appName workflows
	=	{ appName			= appName
		, currentProcessId	= ""
		, currentSession 	= {Session | sessionId = "", user = AnyUser, timestamp = 0}
		, staticWorkflows	= workflows
		}

initIWorld	:: !String !Config !*Store !*World -> *IWorld
initIWorld application config store world
	= 	{ IWorld
		| application		= application
		, config			= config
		, store				= store
		, world				= world
		}
		
initTaskInfo :: TaskInfo
initTaskInfo
	=	{ TaskInfo
		| taskId = ""
		, subject = ""
		, description = ""
		, context = Nothing
		, tags = []
		, groupedBehaviour = GBFixed
		, groupActionsBehaviour = IncludeGroupActions
		}

initSystemProperties :: SystemProperties
initSystemProperties =
	{SystemProperties
	| taskId = ""
	, parent = Nothing
	, status = Active
	, manager = AnyUser
	, issuedAt = Timestamp 0
	, firstEvent = Nothing
	, latestEvent = Nothing
	, latestExtEvent = Nothing
	, subTaskWorkers = []
	, deleteWhenDone = False
	}

initWorkerProperties :: WorkerProperties
initWorkerProperties =
	{WorkerProperties
	| progress = TPActive
	}
	
initTaskProperties :: TaskProperties
initTaskProperties
	= { systemProperties	= initSystemProperties
	  , managerProperties	= initManagerProperties
	  , workerProperties		= initWorkerProperties
	  }

initSession :: !SessionId !*TSt -> (!Maybe String, !*TSt)
initSession sessionId tst
	# (mbSession,timeout,tst=:{staticInfo})	= restoreSession sessionId tst
	= case mbSession of
		Nothing
			= (Just (if timeout "Your session timed out" "Failed to load session"), tst)
		Just session
			= (Nothing, {tst & staticInfo = {staticInfo & currentSession = session}})

createTaskInstance :: !Dynamic !Bool !(Maybe TaskParallelType) !Bool !Bool !*TSt -> (!ProcessId, !TaskResult Dynamic, !TaskTree, !*TSt)	  
createTaskInstance thread=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) toplevel mbParType activate delete tst=:{taskNr,properties}
	//-> the current assigned worker is also the manager of all the tasks IN the process (excluding the main task)
	# (worker,tst)			= getCurrentWorker tst
	# (manager,tst) 		= if (worker <> AnyUser) (worker,tst) (getCurrentUser tst)	
	# (currentTime, tst)	= accWorldTSt time tst
	# taskId				= if toplevel "" (taskNrToString taskNr)
	# parent				= if toplevel Nothing (Just properties.systemProperties.SystemProperties.taskId)
	# managerProperties		= setUser manager (taskProperties originalTask)
	# properties =
		{TaskProperties
		| systemProperties =
			{SystemProperties
			| taskId		= taskId
			, parent		= parent
			, status		= if activate Active Suspended
			, manager		= manager
			, issuedAt		= currentTime
			, firstEvent	= Nothing
			, latestEvent	= Nothing
			, latestExtEvent = Nothing
			, subTaskWorkers = []
			, deleteWhenDone = delete
			}
		, managerProperties = managerProperties
		, workerProperties =
			{WorkerProperties
			| progress	= TPActive
			}
		}
	# process =
		{ Process
		| taskId		 = taskId
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
		# (result,tree,tst)	= evaluateTaskInstance process SpineTree [] Nothing toplevel True {tst & staticInfo = {tst.staticInfo & currentProcessId = processId}}
		= (processId,result,tree,tst)
	| otherwise
		= (processId, TaskBusy, node properties processId, tst)
where
	setUser manager props=:{ManagerProperties|worker=AnyUser} = {ManagerProperties|props & worker = manager}
	setUser manager props = props

	node properties taskId
		# taskNr	= taskNrFromString taskId
		# info =	{ TaskInfo|initTaskInfo
					& taskId	= taskId
					, subject	= properties.managerProperties.ManagerProperties.subject
					}
		= TTMainTask info properties Nothing mbParType (TTFinishedTask info NoOutput)

deleteTaskInstance :: !ProcessId !*TSt -> *TSt
deleteTaskInstance procId tst 
	# (_,tst) 									= deleteProcess procId tst
	# tst=:{TSt | iworld=iworld=:{store,world}}	= deleteSubProcesses procId tst
	# (store,world)								= deleteValues (iTaskId (taskNrFromString procId) "") store world
	= {TSt | tst & iworld = {iworld & world = world, store = store}}

garbageCollectTaskInstance :: !ProcessId !*TSt -> (!Bool,!*TSt)
garbageCollectTaskInstance procId tst
	| tst.TSt.properties.systemProperties.deleteWhenDone
	# tst = deleteTaskInstance procId tst
	= (True,tst)	
	| otherwise
	= (False,tst)

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
storeThread processId thread tst=:{TSt|iworld=iworld=:{IWorld|store}}
	# store = storeValueAs SFDynamic ("iTask_" +++ processId +++ "-thread") thread store
	= {TSt|tst & iworld = {IWorld|iworld & store = store}}

loadThread :: !ProcessId !*TSt -> (!Dynamic,!*TSt)
loadThread processId tst=:{TSt|iworld = iworld =:{IWorld|store,world}}
	# (mbThread, store, world)	= loadValue ("iTask_" +++ processId +++ "-thread") store world
	= case mbThread of
		Just thread	= (thread,{TSt|tst & iworld = {IWorld|iworld & store = store, world = world}})
		Nothing		= abort "Could not load task thread"
		
//END NEW THREAD FUNCTIONS

//Computes a workflow (sub) process
evaluateTaskInstance :: !Process !TreeType ![TaskEvent] !(Maybe ChangeInjection) !Bool !Bool !*TSt-> (!TaskResult Dynamic, !TaskTree, !*TSt)
evaluateTaskInstance process=:{Process | taskId, properties, menus, changeCount, inParallelType} treeType events newChange isTop firstRun tst=:{TSt|currentChange,pendingChanges,tree=parentTree,treeType=parentTreeType,events=parentEvents,properties=parentProperties,menus=parentMenus}
	// Update access timestamps in properties
	# (now,tst)							= accWorldTSt time tst
	# properties						= {properties & systemProperties = {properties.systemProperties
															& latestEvent = Just now
															, firstEvent = case properties.systemProperties.firstEvent of
																Nothing	= Just now
																Just t	= Just t }}
	// Reset the task state
	# tst								= resetTSt taskId treeType events properties inParallelType tst
	// Queue all stored persistent changes (only when run as top node)
	# tst								= if isTop (loadPersistentChanges taskId tst) tst
	// When a change is injected set it as active change
	# tst								= if (isJust newChange) {tst & currentChange = newChange} tst
	// Load the task instance
	# (thread,tst)						= loadThread taskId tst
	// On the first run, apply all changes. Pending persistent ones as well as the currently active change
	// On subsequent runs only apply the currently active change
	// After each application of a change the thread is evaluated because the full evaluated tree must be the
	// subject of later changes
	# (changeCount,thread,properties,menus,result,tst)
										= if firstRun
											(applyAllChanges taskId changeCount pendingChanges thread properties menus tst)
											(applyCurrentChange taskId changeCount thread properties menus tst)
	// The tasktree of this process is the tree as it has been constructed, but with updated properties
	# (TTMainTask ti _ _ _ tasks,tst)	= getTaskTree tst
	# tree								= TTMainTask ti properties menus inParallelType tasks
	// Normalize the tree by evaluating all interactive task nodes that contain functions
	// These can only be evaluated after the complete tree has been evaluated because of mutual dependencies
	# (tree,tst)						= normalizeInteractiveTasks tree tst
	// Store the adapted persistent changes
	# tst								= if isTop (storePersistentChanges taskId tst) tst
	# tst								= restoreTSt parentTree parentTreeType parentEvents parentProperties parentMenus tst
	= case result of
		TaskBusy
			//Update process table (changeCount & properties)
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Active}}
			# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, menus = menus, changeCount = changeCount }) tst
			= (TaskBusy, tree, tst)
		TaskFinished dyn
			//Store result
			# tst 			= storeProcessResult (taskNrFromString taskId) result tst
			//Update process table (status, changeCount & properties)
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Finished}}
			# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, menus = menus, changeCount = changeCount }) tst
			| isTop
				//Evaluate parent process
				| isJust properties.systemProperties.parent
					# (mbParentProcess,tst) = getProcess (fromJust properties.systemProperties.parent) tst
					= case mbParentProcess of
						Nothing 
							= (result,tree,tst)
						Just parentProcess
							# (_,_,tst)	= evaluateTaskInstance parentProcess treeType events Nothing True False tst
							= (result,tree,tst)	
				| otherwise
					= (result,tree,tst)
			| otherwise
				//Update process table (changeCount & properties)
				# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Finished}}
				# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, menus = menus, changeCount = changeCount }) tst
				= (result,tree,tst)
		TaskException e
			//Store exception
			# tst 		= storeProcessResult (taskNrFromString taskId) result tst
			//Update process table
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Excepted}}
			# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, menus = menus, changeCount = changeCount }) tst
			= (TaskException e, tree, tst)
where
	resetTSt :: !TaskId !TreeType ![TaskEvent] !TaskProperties !(Maybe TaskParallelType) !*TSt -> *TSt
	resetTSt taskId treeType events properties inptype tst
		# taskNr	= taskNrFromString taskId
		# info =	{ TaskInfo|initTaskInfo
					& taskId	= taskId
					, subject	= properties.managerProperties.ManagerProperties.subject
					}
		# tree		= TTMainTask info properties menus inptype (TTFinishedTask info NoOutput)
		= {TSt| tst & taskNr = taskNr, tree = tree, treeType = treeType, events = events, staticInfo = {tst.staticInfo & currentProcessId = taskId}}	
	
	restoreTSt :: !TaskTree !TreeType ![TaskEvent] !TaskProperties !(Maybe [Menu]) !*TSt -> *TSt
	restoreTSt tree treeType events properties menus tst = {TSt|tst & tree = tree, treeType = treeType, events = events, properties = properties, menus = menus}
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
		loadChangeFunctions [{PersistentChange|label}:cs] tst=:{TSt|iworld=iworld=:{IWorld|store,world}}
			# (mbDyn, store, world)	= loadValue ("iTask_change-" +++ label) store world
			# (dyns,tst)	= loadChangeFunctions cs {TSt|tst & iworld = {IWorld|iworld & store = store, world = world}}
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
												# tst=:{TSt|iworld=iworld=:{IWorld|store,world}}
																	= deleteChange label tst
												# (store,world)		= deleteValues ("iTask_change-" +++ label) store world 
												
												= {tst & iworld = {IWorld| iworld & store = store, world = world}, currentChange = Nothing}
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
	setTaskContext cxt (Task props gprops _ tf) = Task props gprops (Just cxt) tf
	
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
		storeChanges [(label,dyn):cs] tst=:{TSt|iworld=iworld=:{IWorld|store}} 
			# store	= storeValueAs SFDynamic ("iTask_change-" +++ label) dyn store
			= storeChanges cs {TSt|tst & iworld = {IWorld|iworld & store = store}} 

	/*
	* Evaluate all functions in interactive task nodes
	*/
	normalizeInteractiveTasks :: !TaskTree !*TSt -> (!TaskTree,!*TSt)
	//The interactive task leaves that are normalized
	normalizeInteractiveTasks (TTInteractiveTask ti it) tst
		= case it of
			(UIOutput (Func f))
				# (it,tst) = f tst
				= (TTInteractiveTask ti (UIOutput it), tst)
			_
				= (TTInteractiveTask ti it, tst)
	//For grouped tasks the actions are also normalized
	normalizeInteractiveTasks (TTGroupedTask ti tasks actions s) tst
		# (actions,tst)	= mapSt normalizeAction actions tst
		# (tasks,tst)	= mapSt normalizeInteractiveTasks tasks tst
		= (TTGroupedTask ti tasks actions s, tst)
	where
		normalizeAction (action, Left b) tst = ((action,Left b), tst)
		normalizeAction (action, Right f) tst
			# (b,tst)	= f tst
			= ((action,Left b), tst)
	//Tree traversal
	normalizeInteractiveTasks (TTMainTask ti properties menus pt task) tst
		# (task,tst) = normalizeInteractiveTasks task tst
		= (TTMainTask ti properties menus pt task, tst)
	normalizeInteractiveTasks (TTSequenceTask ti tasks) tst
		# (tasks,tst)	= mapSt normalizeInteractiveTasks tasks tst
		= (TTSequenceTask ti tasks, tst)
	normalizeInteractiveTasks (TTParallelTask ti pi tasks) tst
		# (tasks,tst)	= mapSt normalizeInteractiveTasks tasks tst
		= (TTParallelTask ti pi tasks, tst)
	
	//All other leaf cases
	normalizeInteractiveTasks tree tst = (tree,tst)
	
mapSt f [] st = ([], st)
mapSt f [x:xs] st
	# (y, st) = f x st
	# (ys, st) = mapSt f xs st
	= ([y:ys], st)
	
applyChangeToTaskTree :: !ProcessId !ChangeInjection !*TSt -> *TSt
applyChangeToTaskTree pid (lifetime,change) tst=:{taskNr,taskInfo,tree,staticInfo,currentChange,pendingChanges, properties, menus}
	# (mbProcess,tst) = getProcess pid tst
	= case mbProcess of
		(Just proc) 
			# (_,_,tst) = evaluateTaskInstance proc SpineTree [] (Just (lifetime,change)) True False tst
			= {tst & taskNr = taskNr, taskInfo = taskInfo,properties = properties, menus = menus
			  , tree = tree, staticInfo = staticInfo, currentChange = currentChange, pendingChanges = pendingChanges}
		Nothing		
			= tst

calculateTaskResult :: !TaskId !*TSt -> (!TaskTree, !*TSt)
calculateTaskResult taskId tst
	# (mbProcess,tst) = getProcessForTask taskId tst
	= case mbProcess of
		Nothing
			# info =	{ TaskInfo| initTaskInfo
						& taskId			= taskId
						, subject			= "Unknown Process"
						, description		= "Task Result"
						}
			= (TTFinishedTask info NoOutput, tst)
		Just process=:{Process|properties}
			# tst=:{TSt | iworld=iworld=:{store,world}} = tst
			# (mbContainer,store,world) = loadValue (iTaskId taskId "container") store world
			# result = case mbContainer of
				(Just dyn) = UIOutput (renderResult dyn)
				(Nothing)  = UIOutput [Text "Cannot load result."]
			# info = { TaskInfo|initTaskInfo
			 		 & taskId = taskId
			 		 , subject = properties.managerProperties.ManagerProperties.subject
			 		 , description = "Task Result"
			 		 }
			= (TTFinishedTask info result, {TSt | tst & iworld = {IWorld | iworld & store = store, world = world}})

calculateTaskTree :: !TaskId !TreeType ![TaskEvent] !*TSt -> (!TaskTree, !*TSt)
calculateTaskTree taskId treeType events tst
	# (mbProcess,tst) = getProcess taskId tst
	= case mbProcess of
		Nothing
			# info =	{ TaskInfo | initTaskInfo
						& taskId			= taskId
						, subject			= "Deleted Process"
						, description		= "Task Result"
						}
			= (TTFinishedTask info NoOutput, tst)
		Just process=:{Process|properties}
			= case properties.systemProperties.SystemProperties.status of
				Active
					//Evaluate the process
					# (result,tree,tst) = evaluateTaskInstance process treeType events Nothing True False tst
					= (tree,tst)
				_		
					//retrieve process result from store and show it??
					# tst=:{TSt | iworld=iworld=:{store,world}} = tst
					# (mbContainer,store,world) = loadValue (iTaskId taskId "container") store world				
					# result = case mbContainer of
						(Just dyn)	= UIOutput (renderResult dyn)
						(Nothing)	= UIOutput [Text "Cannot load result."]
					# info =	{ TaskInfo| initTaskInfo
								& taskId			= taskId
								, subject			= properties.managerProperties.ManagerProperties.subject
								, description		= "Task Result"
								}
					= (TTFinishedTask info result, {TSt | tst & iworld = {IWorld | iworld & store = store, world = world}})

renderResult :: Dynamic -> [HtmlTag]
renderResult (Container value :: Container a a) = visualizeAsHtmlDisplay value				

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
	= (properties.managerProperties.ManagerProperties.worker,{TSt | tst & properties = properties})

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

appIWorldTSt :: !.(*IWorld -> *IWorld) !*TSt -> *TSt
appIWorldTSt f tst=:{TSt|iworld}
	= {TSt|tst & iworld = f iworld}
	
accIWorldTSt :: !.(*IWorld -> *(.a,*IWorld))!*TSt -> (.a,!*TSt)
accIWorldTSt f tst=:{TSt|iworld}
	# (a,iworld) = f iworld
	= (a,{TSt|tst & iworld = iworld})


appWorldTSt	:: !.(*World -> *World) !*TSt -> *TSt
appWorldTSt f tst=:{TSt|iworld=iworld=:{IWorld|world}}
	= {TSt|tst & iworld = {IWorld|iworld & world = f world}}

accWorldTSt	:: !.(*World -> *(.a,*World))!*TSt -> (.a,!*TSt)
accWorldTSt f tst=:{TSt|iworld=iworld=:{IWorld|world}}
	# (a,world) = f world
	= (a, {TSt|tst & iworld = {IWorld|iworld & world = world}})

mkTaskFunction :: (*TSt -> (!a,!*TSt)) -> (*TSt -> (!TaskResult a,!*TSt))
mkTaskFunction f = \tst -> let (a,tst`) = f tst in (TaskFinished a,tst`)
		
mkInteractiveTask :: !String !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a 
mkInteractiveTask subject description taskfun = Task {ManagerProperties|initManagerProperties & subject = subject, description = description} initGroupedProperties Nothing mkInteractiveTask`	
where
	mkInteractiveTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTInteractiveTask taskInfo NoOutput}

mkInstantTask :: !String !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkInstantTask subject description taskfun = Task {ManagerProperties|initManagerProperties & subject = subject, description = description} initGroupedProperties Nothing mkInstantTask`
where
	mkInstantTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTFinishedTask taskInfo NoOutput} //We use a FinishedTask node because the task is finished after one evaluation

mkMonitorTask :: !String !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkMonitorTask subject description taskfun = Task {ManagerProperties|initManagerProperties & subject = subject, description = description} initGroupedProperties Nothing mkMonitorTask`
where
	mkMonitorTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTMonitorTask taskInfo NoOutput}

mkInstructionTask :: !String !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkInstructionTask subject description taskfun = Task {ManagerProperties|initManagerProperties & subject = subject, description = description} initGroupedProperties Nothing mkInstructionTask`
where
	mkInstructionTask` tst =:{TSt | taskInfo}
		= taskfun {tst & tree = TTInstructionTask taskInfo NoOutput}

mkRpcTask :: !String !RPCExecute !(String -> a) -> Task a | gUpdate{|*|} a
mkRpcTask taskname rpce parsefun = Task {ManagerProperties|initManagerProperties & subject = taskname} initGroupedProperties Nothing mkRpcTask`
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
	# (mbMsg) = fromJSON (fromString v)
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
			# tst=:{TSt|iworld}	
										= tst
			# (def,iworld)				= defaultValue iworld
			= (TaskFinished def, {TSt|tst & iworld = iworld})
			
	setStatus "" tst		= tst
	setStatus status tst	= setTaskStore "status" status tst
		
mkSequenceTask :: !String !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkSequenceTask subject description taskfun = Task {ManagerProperties|initManagerProperties & subject = subject, description = description} initGroupedProperties Nothing mkSequenceTask`
where
	mkSequenceTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTSequenceTask taskInfo [], taskNr = [0:taskNr]}
			
mkParallelTask :: !String !String !TaskParallelType !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkParallelTask subject description tpt taskfun = Task {ManagerProperties|initManagerProperties & subject = subject, description = description} initGroupedProperties Nothing mkParallelTask`
where
	mkParallelTask` tst=:{TSt|taskNr,taskInfo}
		# tst = {tst & tree = TTParallelTask taskInfo tpt []}												
		= taskfun tst

mkGroupedTask :: !String !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkGroupedTask subject description taskfun = Task {ManagerProperties|initManagerProperties & subject = subject, description = description} initGroupedProperties Nothing mkGroupedTask`
where
	mkGroupedTask` tst=:{TSt|taskNr,taskInfo}
		# tst = {tst & tree = TTGroupedTask taskInfo [] [] Nothing, taskNr = [0:taskNr]}
		= taskfun tst
			
mkMainTask :: !String !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
mkMainTask subject description taskfun = Task {ManagerProperties|initManagerProperties & subject = subject, description = description} initGroupedProperties Nothing mkMainTask`
where
	mkMainTask` tst=:{taskNr,taskInfo}
		= taskfun {tst & tree = TTMainTask taskInfo initTaskProperties Nothing Nothing (TTFinishedTask taskInfo NoOutput)}

applyTask :: !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a
applyTask (Task initProperties groupedProperties mbInitTaskNr taskfun) tst=:{taskNr,tree,properties,iworld=iworld=:{IWorld|store,world}}
	# taskId								= iTaskId taskNr ""
	# (taskVal,store,world)					= loadValue taskId store world
	# taskInfo =	{ TaskInfo
					| taskId				= taskNrToString taskNr
					, subject				= initProperties.ManagerProperties.subject
					, description			= initProperties.ManagerProperties.description
					, context				= Nothing
					, tags					= initProperties.ManagerProperties.tags
					, groupedBehaviour 		= groupedProperties.GroupedProperties.groupedBehaviour
					, groupActionsBehaviour	= groupedProperties.GroupedProperties.groupActionsBehaviour
					}
	# tst = {TSt|tst & taskInfo = taskInfo, newTask = isNothing taskVal, iworld = {IWorld| iworld & store = store, world = world }}
	= case taskVal of
		(Just (TaskFinished a))	
			# tst = addTaskNode (TTFinishedTask taskInfo (UIOutput (visualizeAsHtmlDisplay a))) tst
			= (TaskFinished a, {tst & taskNr = incTaskNr taskNr})
		_
			// If the task is new, but has run in a different context, initialize the states of the task and its subtasks
			# tst	= case (taskVal,mbInitTaskNr) of
						(Nothing, Just initTaskNr)	= copyTaskStates initTaskNr taskNr tst
						_							= tst
			// Execute task function
			# (result, tst=:{tree=node,iworld=iworld=:{IWorld|store}})	= taskfun tst
			// Update task state
			= case result of
				(TaskFinished a)
					//If a process is finished (tl taskNr == procId), remove the process from the process DB (if it's allowed to be deleted)
					# procId				= taskNrToString (tl taskNr)
					# (gc,tst)				= if(procId == tst.TSt.properties.systemProperties.SystemProperties.taskId)
													(garbageCollectTaskInstance procId {TSt | tst & tree=node, iworld = {IWorld | iworld & store = store}})
													(False,tst)
					//Garbage collect task store
					# tst=:{TSt|iworld =iworld=:{IWorld|store}}	= deleteTaskStates taskNr tst					
					// Store final value if the process is not garbage collected
					# store					= if(gc) store (storeValue taskId result store)
					// Store the final value and it's type as a dynamic value, so it can be visualized by the task-result service later.
					# store					= if(gc) store (storeValueAs SFDynamic (taskId+++"-container") (dynamic (Container a) :: Container a^ a^) store)
					# tst					= addTaskNode (TTFinishedTask taskInfo (UIOutput (visualizeAsHtmlDisplay a)))
												{tst & taskNr = incTaskNr taskNr, tree = tree, iworld = {IWorld|iworld & store = store}}
					= (TaskFinished a, tst)
				(TaskBusy)
					// Store intermediate value
					# procId				= taskNrToString (tl taskNr)	
					# store					= storeValue taskId result store
					# tst					= addTaskNode (finalizeTaskNode node)
												{tst & taskNr = incTaskNr taskNr, tree = tree, iworld = {IWorld|iworld & store = store}}
					= (TaskBusy, tst)
				(TaskException e)
					// Store exception
					# store					= storeValue taskId result store
					# tst					= addTaskNode (TTFinishedTask taskInfo (UIOutput [Text "Uncaught exception"]))
												{tst & taskNr = incTaskNr taskNr, tree = tree, iworld = {IWorld|iworld & store = store}}
					= (TaskException e, tst)
		
where
	//Increase the task nr
	incTaskNr [] = [0]
	incTaskNr [i:is] = [i+1:is]
	
	//Perform reversal of lists that have been accumulated in reversed order
	finalizeTaskNode (TTSequenceTask ti tasks) 					= TTSequenceTask	ti (reverse tasks)
	finalizeTaskNode (TTParallelTask ti tpi tasks)				= TTParallelTask	ti tpi (reverse tasks)
	finalizeTaskNode (TTGroupedTask ti tasks gActions mbFocus)	= TTGroupedTask		ti (reverse tasks) gActions mbFocus
	finalizeTaskNode node										= node

//Add a new node to the current sequence or process
addTaskNode :: !TaskTree !*TSt -> *TSt
addTaskNode node tst=:{tree} = case tree of
	(TTMainTask ti mti menus inptype task)		= {tst & tree = TTMainTask ti mti menus inptype node} 			//Just replace the subtree 
	(TTSequenceTask ti tasks)					= {tst & tree = TTSequenceTask ti [node:tasks]}					//Add the node to the sequence
	(TTParallelTask ti tpi tasks)				= {tst & tree = TTParallelTask ti tpi [node:tasks]}				//Add the node to the parallel set
	(TTGroupedTask ti tasks gActions mbFocus)	= {tst & tree = TTGroupedTask ti [node:tasks] gActions mbFocus}	//Add the node to the grouped set
	_											= {tst & tree = tree}
		
setTUIDef	:: !([TUIDef],[TUIButton]) ![(Action,Bool)] !*TSt -> *TSt
setTUIDef def accActions tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask info (UIOutput (Definition def accActions))}
		_								= tst

setTUIUpdates :: ![TUIUpdate] ![(Action,Bool)] !*TSt -> *TSt
setTUIUpdates upd accActions tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask info (UIOutput (Updates upd accActions))}
		_								= tst
		
setTUIFunc :: (*TSt -> *(!InteractiveTask, !*TSt)) !*TSt -> *TSt
setTUIFunc func tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask info (UIOutput (Func func))}
		_								= tst

setTUIMessage :: !([TUIDef],[TUIButton]) ![(Action,Bool)] !*TSt -> *TSt
setTUIMessage msg accActions tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask info (UIOutput (Message msg accActions))}
		_								= tst

setStatus :: ![HtmlTag] !*TSt -> *TSt
setStatus msg tst=:{tree}
	= case tree of
		(TTMonitorTask info _)				= {tst & tree = TTMonitorTask info (UIOutput msg)}
		_									= tst

setInstruction :: !(Maybe [HtmlTag]) !*TSt -> *TSt
setInstruction context tst=:{tree}
	= case tree of
			(TTInstructionTask ti _)	= {tst & tree = TTInstructionTask ti (UIOutput context)}
			_							= tst
			
setGroupActions :: ![(Action, (Either Bool (*TSt -> *(!Bool,!*TSt))))] !*TSt -> *TSt
setGroupActions actions tst=:{tree}
	= case tree of
		(TTGroupedTask info tasks _ mbFocus)	= {tst & tree = TTGroupedTask info tasks actions mbFocus}
		_										= tst
		
setFocusCommand :: !String !*TSt -> *TSt
setFocusCommand tag tst=:{tree}
	= case tree of
		(TTGroupedTask info tasks actions _)	= {tst & tree = TTGroupedTask info tasks actions (Just tag)}
		_										= tst

setJSONValue :: !JSONNode !*TSt -> *TSt
setJSONValue json tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)				= {tst & tree = TTInteractiveTask info (JSONOutput json)}

/**
* Store and load the result of a workflow instance
*/
loadProcessResult :: !TaskNr !*TSt -> (!Maybe (TaskResult Dynamic), !*TSt)
loadProcessResult taskNr tst =:{TSt|iworld=iworld=:{IWorld|store, world}}
	# (mbResult, store, world) = loadValue key store world
	= (mbResult, {TSt | tst & iworld = {IWorld|iworld & store = store, world = world}})
where
	key = "iTask_"+++(taskNrToString taskNr)+++"-result"
	
storeProcessResult :: !TaskNr !(TaskResult Dynamic) !*TSt -> *TSt
storeProcessResult taskNr result tst
	// Only store process result if the process is not garbage collected (still exists in te process table)
	# (storeResult,tst=:{TSt|iworld=iworld=:{IWorld|store}}) = storeResult tst
	# store	= if(storeResult) (storeValueAs SFDynamic key result store) store
	= {TSt |tst & iworld = {IWorld|iworld & store = store}}
where
	key = "iTask_"+++(taskNrToString taskNr)+++"-result"
	
	storeResult tst 
		# (mbproc,tst) = getProcess (taskNrToString taskNr) tst
		= (isJust mbproc, tst) 
		
	
setTaskStore :: !String !a !*TSt -> *TSt | iTask a
setTaskStore key value tst=:{taskNr}
	= setTaskStoreFor taskNr key value tst
	
setTaskStoreFor :: !TaskNr !String !a !*TSt -> *TSt | iTask a
setTaskStoreFor taskNr key value tst=:{TSt|iworld=iworld=:{IWorld|store}}
	# store = storeValue storekey value store
	= {TSt|tst & iworld = {IWorld| iworld & store = store}}
where
	storekey = "iTask_" +++ (taskNrToString taskNr) +++ "-" +++ key

getTaskStore :: !String !*TSt -> (Maybe a, !*TSt) | iTask a
getTaskStore key tst=:{taskNr}
	= getTaskStoreFor taskNr key tst

getTaskStoreFor	:: !TaskNr !String !*TSt -> (Maybe a, !*TSt) | iTask a
getTaskStoreFor taskNr key tst=:{TSt|iworld=iworld=:{IWorld|store,world}}
	# (mbValue,store,world) = loadValue storekey store world
	= (mbValue,{TSt|tst& iworld = {IWorld|iworld & store = store, world = world}})
where
	storekey = "iTask_" +++ (taskNrToString taskNr) +++ "-" +++ key

getEvents :: !*TSt -> ([(!String,!String)],!*TSt)
getEvents tst=:{taskNr,events}
	# (matched, rest)	= getEvents` events
	= (matched, {TSt|tst & events = rest})
where
	taskId = taskNrToString taskNr

	getEvents` [] = ([],[])
	getEvents` [event=:(task,name,value):events]
		# (matched,rest) = getEvents` events
		| task == taskId
			| name == "menuAndGroup"	= ([(name,value):matched], [(task,"group",value):rest]) 
			| name <> "group"			= ([(name,value):matched], rest)
			| otherwise					= (matched, [event:rest])
		| otherwise						= (matched, [event:rest])

getGroupEvents :: !TaskId !*TSt -> ([(!String,!String)],!*TSt)
getGroupEvents taskId tst=:{TSt|events}
	# (matched, rest) = getGroupEvents` events
	= (matched, {TSt|tst & events = rest})
where
	getGroupEvents` []
		= ([],[])											
	getGroupEvents` [event=:(task,name,value):events]
		# (matched,rest) = getGroupEvents` events
		| (startsWith taskId task) && (name == "group" || name == "menuAndGroup")
			= ([(name,value):matched], [event:rest])
		| otherwise
			= (matched, [event:rest])

anyEvents :: !*TSt -> (!Bool,!*TSt)
anyEvents tst=:{TSt|events} = (not (isEmpty events),tst)

resetSequence :: !*TSt -> *TSt
resetSequence tst=:{taskNr,tree}
	= case tree of
		(TTSequenceTask info sequence)	= {tst & taskNr = [0:tl taskNr], tree = TTSequenceTask info []}
		_								= {tst & tree = tree}

deleteTaskStates :: !TaskNr !*TSt -> *TSt
deleteTaskStates taskNr tst=:{TSt|iworld=iworld=:{IWorld|store,world}}
	// Delete values in the data store
	# (store,world) 	= deleteValues (iTaskId taskNr "") store world
	= {TSt|tst & iworld = {IWorld|iworld & world = world, store = store}}
	
copyTaskStates :: !TaskNr !TaskNr !*TSt	-> *TSt
copyTaskStates fromtask totask tst=:{TSt|iworld=iworld=:{IWorld|store,world}}
	// Copy values in the data store
	# (store,world) 	= copyValues (iTaskId fromtask "") (iTaskId totask "") store world
	// Copy subprocess in the process table
	# tst				= copySubProcesses (taskNrToString fromtask) (taskNrToString totask) {TSt|tst & iworld = {IWorld|iworld & store = store, world = world}}
	= tst
	
flushStore :: !*TSt -> *TSt
flushStore tst=:{TSt|iworld=iworld=:{IWorld|store,world}}
	# (store,world) = flushCache store world
	= {TSt|tst & iworld = {IWorld|iworld & store = store, world = world}}

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

events2Paths :: ![(!String,!String)] -> [DataPath]
events2Paths updates = map (s2dp o fst) updates