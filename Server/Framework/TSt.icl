implementation module TSt

import StdEnv, Maybe
import HTTP, Util, Text
import ProcessDB, SessionDB, ChangeDB, DocumentDB, UserDB, TaskTree
import GenEq, GenVisualize, GenUpdate, Store, Config, dynamic_string
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
		, tree			= TTMainTask initTaskInfo initTaskProperties Nothing (TTFinishedTask initTaskInfo NoOutput)
		, treeType		= SpineTree
		, newTask		= True
		, events		= []
		, properties	= initTaskProperties
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
		, currentSession 	= {Session | sessionId = "", user = AnyUser, timestamp = Timestamp 0}
		, staticWorkflows	= workflows
		}

initIWorld	:: !String !Config !*Store !*World -> *IWorld
initIWorld application config store world
	# (timestamp,world) = time world
	= 	{ IWorld
		| application		= application
		, config			= config
		, store				= store
		, world				= world
		, timestamp			= timestamp
		}
		
initTaskInfo :: TaskInfo
initTaskInfo
	=	{ TaskInfo
		| taskId = ""
		, subject = ""
		, description = ""
		, context = Nothing
		, tags = []
		, groupedBehaviour = Fixed
		, groupActionsBehaviour = IncludeGroupActions
		, menus = Nothing
		, formWidth = Nothing
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
	  , workerProperties	= initWorkerProperties
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
createTaskInstance thread=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) toplevel mbParType activate delete tst=:{taskNr,properties,iworld=iworld=:{IWorld|timestamp=currentTime}}
	//-> the current assigned worker is also the manager of all the tasks IN the process (excluding the main task)
	# (worker,tst)			= getCurrentWorker tst
	# (manager,tst) 		= if (worker <> AnyUser) (worker,tst) (getCurrentUser tst)	
	# taskId				= if toplevel "" (taskNrToString taskNr)
	# parent				= if toplevel Nothing (Just properties.systemProperties.SystemProperties.taskId)
	# managerProperties		= setUser manager (taskProperties originalTask)
	# properties =
		{systemProperties =
			{ taskId		= taskId
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
			{ progress	= TPActive
			}
		}
	# process =
		{ Process
		| taskId		 = taskId
		, properties	 = properties
		, changeCount	 = 0
		, mutable		 = True
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
	setUser manager props=:{worker=AnyUser} = {props & worker = manager}
	setUser manager props = props

	node properties taskId
		# taskNr	= taskNrFromString taskId
		# info =	{ TaskInfo|initTaskInfo
					& taskId	= taskId
					, subject	= properties.managerProperties.ManagerProperties.taskDescription.TaskDescription.title
					}
		= TTMainTask info properties mbParType (TTFinishedTask info NoOutput)

deleteTaskInstance :: !ProcessId !*TSt -> *TSt
deleteTaskInstance procId tst 
	# (_,tst) 	= deleteProcess procId tst
	# tst		= deleteSubProcesses procId tst
	# tst		= appIWorldTSt (deleteValues (iTaskId (taskNrFromString procId) "")) tst
	= tst

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
storeThread processId thread tst
	= appIWorldTSt (storeValueAs SFDynamic ("iTask_" +++ processId +++ "-thread") thread) tst

loadThread :: !ProcessId !*TSt -> (!Dynamic,!*TSt)
loadThread processId tst
	# (mbThread,tst) = accIWorldTSt (loadValue ("iTask_" +++ processId +++ "-thread")) tst
	= case mbThread of
		Just thread	= (thread,tst)
		Nothing		= abort "Could not load task thread"
		
//END NEW THREAD FUNCTIONS

//Computes a workflow (sub) process
evaluateTaskInstance :: !Process !TreeType ![TaskEvent] !(Maybe ChangeInjection) !Bool !Bool !*TSt-> (!TaskResult Dynamic, !TaskTree, !*TSt)
evaluateTaskInstance process=:{Process | taskId, properties, changeCount, inParallelType} treeType events newChange isTop firstRun tst=:{TSt|currentChange,pendingChanges,tree=parentTree,treeType=parentTreeType,events=parentEvents,properties=parentProperties,iworld=iworld=:{IWorld|timestamp=now}}
	// Update access timestamps in properties
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
	# (changeCount,thread,properties,result,tst)
										= if firstRun
											(applyAllChanges taskId changeCount pendingChanges thread properties tst)
											(applyCurrentChange taskId changeCount thread properties tst)
	// The tasktree of this process is the tree as it has been constructed, but with updated properties
	# (TTMainTask ti _ _ tasks,tst)		= getTaskTree tst
	# tree								= TTMainTask ti properties inParallelType tasks
	// Normalize the tree by evaluating all interactive task nodes that contain functions
	// These can only be evaluated after the complete tree has been evaluated because of mutual dependencies
	# (tree,tst)						= normalizeTasks tree tst
	// Store the adapted persistent changes
	# tst								= if isTop (storePersistentChanges taskId tst) tst
	# tst								= restoreTSt parentTree parentTreeType parentEvents parentProperties tst
	= case result of
		TaskBusy
			//Update process table (changeCount & properties)
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Active}}
			# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, changeCount = changeCount}) tst
			= (TaskBusy, tree, tst)
		TaskFinished dyn
			//Store result
			# tst 			= storeProcessResult (taskNrFromString taskId) result tst
			//Store result container
			# tst			= storeProcessContainer taskId result thread tst
			//Update process table (status, changeCount & properties)
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Finished}}
			# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, changeCount = changeCount}) tst
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
				# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, changeCount = changeCount}) tst
				= (result,tree,tst)
		TaskException e
			//Store exception
			# tst 		= storeProcessResult (taskNrFromString taskId) result tst
			//Update process table
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Excepted}}
			# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, changeCount = changeCount}) tst
			= (TaskException e, tree, tst)
where
	resetTSt :: !TaskId !TreeType ![TaskEvent] !TaskProperties !(Maybe TaskParallelType) !*TSt -> *TSt
	resetTSt taskId treeType events properties inptype tst
		# taskNr	= taskNrFromString taskId
		# info =	{ TaskInfo|initTaskInfo
					& taskId	= taskId
					, subject	= properties.managerProperties.ManagerProperties.taskDescription.TaskDescription.title
					}
		# tree		= TTMainTask info properties inptype (TTFinishedTask info NoOutput)
		= {TSt| tst & taskNr = taskNr, tree = tree, treeType = treeType, events = events, staticInfo = {tst.staticInfo & currentProcessId = taskId}}	
	
	restoreTSt :: !TaskTree !TreeType ![TaskEvent] !TaskProperties !*TSt -> *TSt
	restoreTSt tree treeType events properties tst = {TSt|tst & tree = tree, treeType = treeType, events = events, properties = properties}
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
		loadChangeFunctions [{PersistentChange|label}:cs] tst
			# (mbDyn,tst)	= accIWorldTSt (loadValue ("iTask_change-" +++ label)) tst
			# (dyns,tst)	= loadChangeFunctions cs tst
			= case mbDyn of
				Nothing		= (dyns,tst)
				Just dyn	= ([(CLPersistent label,dyn):dyns],tst)
	/*
	* When a (sub)process is evaluated for the first time, all existing persistent changes need to be applied.
	* Because applying a change may result in the creation of new sub processes, changes must be carefully applied
	* in the right order. 
	*/
	applyAllChanges :: !ProcessId !Int [(!ChangeLifeTime,!Dynamic)] !Dynamic !TaskProperties !*TSt -> (!Int, !Dynamic, !TaskProperties, !TaskResult Dynamic, !*TSt)
	applyAllChanges processId changeCount [] thread properties tst
		//Only apply the current change
		= applyCurrentChange processId changeCount thread properties tst
	applyAllChanges processId changeCount changes thread properties tst=:{currentChange}
		//Add the pending changes one after another (start empty)
		= applyAllChanges` processId changeCount currentChange changes thread properties {tst & pendingChanges = []} 
	where
		applyAllChanges` processId changeCount currentChange [] thread properties tst
			= applyCurrentChange processId changeCount thread properties {tst & currentChange = currentChange}
		applyAllChanges` processId changeCount currentChange [c:cs] thread properties tst
			# (changeCount,thread,properties,result,tst) = applyCurrentChange processId changeCount thread properties {tst & currentChange = Just c} 
			//Update pending changes list
			# tst	= {tst & pendingChanges = (case tst.currentChange of
														Nothing = tst.pendingChanges
														Just change = tst.pendingChanges ++ [change]) }	
			= case result of
				TaskBusy
					//Continue applying changes
					= applyAllChanges` processId changeCount currentChange cs thread properties tst
				TaskFinished val
					//A change caused the task to complete. Stop, but keep pending changes
					= (changeCount,thread,properties,result,{tst & pendingChanges = tst.pendingChanges ++ cs, currentChange = currentChange})
				TaskException e
					//A change caused an exception. Stop, but keep pending changes
					= (changeCount,thread,properties,result,{tst & pendingChanges = tst.pendingChanges ++ cs, currentChange = currentChange})

	applyCurrentChange :: !ProcessId !Int !Dynamic !TaskProperties !*TSt -> (!Int, !Dynamic, !TaskProperties, !TaskResult Dynamic, !*TSt)
	applyCurrentChange processId changeCount thread properties tst=:{currentChange}
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
												# tst	= deleteChange label tst
												# tst	= appIWorldTSt (deleteValues ("iTask_change-" +++ label)) tst
												= {tst & currentChange = Nothing}
											_
												= {tst & currentChange = Nothing}
									Just change
										= {tst & currentChange = Just (lifetime,change)}
				// Evaluate the thread
				// IMPORTANT: The taskNr is reset with the latest change count
				# (result,tst=:{TSt|properties})
					= applyThread thread {TSt|tst & taskNr = [changeCount: taskNrFromString processId], properties = properties}
				= (changeCount,thread,properties,result,{TSt|tst & properties = properties})
			Nothing
				# (result,tst=:{TSt|properties})
					= applyThread thread {TSt|tst & taskNr = [changeCount: taskNrFromString processId], properties = properties}
				= (changeCount,thread,properties,result,{TSt|tst & properties = properties})
	
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
	setTaskContext cxt task = {task & mbTaskNr = Just cxt}
	
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
		storeChanges [(label,dyn):cs] tst
			# tst = appIWorldTSt (storeValueAs SFDynamic ("iTask_change-" +++ label) dyn) tst
			= storeChanges cs tst

	/*
	* Evaluate all functions in interactive task nodes and task infos
	*/
	normalizeTasks :: !TaskTree !*TSt -> (!TaskTree,!*TSt)
	//The interactive task leaves that are normalized
	normalizeTasks (TTInteractiveTask ti it) tst
		# (ti, tst) = normalizeTaskInfo ti tst
		= case it of
			UIOutput (Func f)
				# (it,tst) = accIWorldTSt f tst
				= (TTInteractiveTask ti (UIOutput it), tst)
			JSONOutput (JSONFunc f)
				# (json,tst) = accIWorldTSt f tst
				= (TTInteractiveTask ti (JSONOutput (JSONValue json)), tst)
			_
				= (TTInteractiveTask ti it, tst)
	//For grouped tasks the actions are also normalized
	normalizeTasks (TTGroupedTask ti tasks actions s) tst
		# (ti, tst)		= normalizeTaskInfo ti tst
		# (actions,tst)	= mapSt normalizeAction actions tst
		# (tasks,tst)	= mapSt normalizeTasks tasks tst
		= (TTGroupedTask ti tasks actions s, tst)
	where
		normalizeAction (action, Left b) tst = ((action,Left b), tst)
		normalizeAction (action, Right f) tst
			# (b,tst)	= accIWorldTSt f tst
			= ((action,Left b), tst)
	//Tree traversal
	normalizeTasks (TTMainTask ti properties pt task) tst
		# (ti, tst)		= normalizeTaskInfo ti tst
		# (task,tst)	= normalizeTasks task tst
		= (TTMainTask ti properties pt task, tst)
	normalizeTasks (TTSequenceTask ti tasks) tst
		# (ti, tst)		= normalizeTaskInfo ti tst
		# (tasks,tst)	= mapSt normalizeTasks tasks tst
		= (TTSequenceTask ti tasks, tst)
	normalizeTasks (TTParallelTask ti pi tasks) tst
		# (ti, tst)		= normalizeTaskInfo ti tst
		# (tasks,tst)	= mapSt normalizeTasks tasks tst
		= (TTParallelTask ti pi tasks, tst)
	
	//All other leaf cases
	normalizeTasks (TTInstructionTask ti to) tst
		# (ti, tst) = normalizeTaskInfo ti tst
		= (TTInstructionTask ti to, tst)
	normalizeTasks (TTMonitorTask ti to) tst
		# (ti, tst) = normalizeTaskInfo ti tst
		= (TTMonitorTask ti to, tst)
	normalizeTasks (TTFinishedTask ti to) tst
		# (ti, tst) = normalizeTaskInfo ti tst
		= (TTFinishedTask ti to, tst)
	normalizeTasks (TTRpcTask ti rpc) tst
		# (ti, tst) = normalizeTaskInfo ti tst
		= (TTRpcTask ti rpc, tst)
		
normalizeTaskInfo ti=:{TaskInfo | menus} tst=:{TSt | iworld}
	# (menus, iworld) = case menus of
		Just (GenFunc f)	= app2 (Just o Menus, id) (f iworld)
		m					= (m, iworld)
	= ({ti & menus = menus}, {TSt | tst & iworld = iworld})

applyChangeToTaskTree :: !ProcessId !ChangeInjection !*TSt -> *TSt
applyChangeToTaskTree pid (lifetime,change) tst=:{taskNr,taskInfo,tree,staticInfo,currentChange,pendingChanges, properties}
	# (mbProcess,tst) = getProcess pid tst
	= case mbProcess of
		(Just proc) 
			# (_,_,tst) = evaluateTaskInstance proc SpineTree [] (Just (lifetime,change)) True False tst
			= {tst & taskNr = taskNr, taskInfo = taskInfo,properties = properties
			  , tree = tree, staticInfo = staticInfo, currentChange = currentChange, pendingChanges = pendingChanges}
		Nothing		
			= tst

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
					# (mbContainer,tst) = accIWorldTSt (loadValue (taskId +++ "-container")) tst				
					# result = case mbContainer of
						(Just dyn)	= UIOutput (renderResult dyn)
						(Nothing)	= UIOutput (Text "Cannot load result.")
					# info =	{ TaskInfo| initTaskInfo
								& taskId			= taskId
								, subject			= properties.managerProperties.ManagerProperties.taskDescription.TaskDescription.title
								, description		= "Task Result"
								}
					= (TTFinishedTask info result,tst)

renderResult :: Dynamic -> HtmlTag
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
	= (properties.managerProperties.worker,{TSt | tst & properties = properties})

getTaskTree :: !*TSt	-> (!TaskTree, !*TSt)
getTaskTree tst =: {tree}
	= (tree, {tst & tree = tree})

getConfigSetting :: !(Config -> a) !*TSt -> (!a,!*TSt)
getConfigSetting f tst
	= accIWorldTSt getFromIWorld tst
where
	getFromIWorld iworld=:{IWorld|config}
		= (f config, {IWorld|iworld & config = config})

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
		
mkInteractiveTask :: !d !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkInteractiveTask description taskfun =
	{ taskProperties	= {ManagerProperties|initManagerProperties & taskDescription = toDescr description}
	, groupedProperties	= initGroupedProperties
	, mbTaskNr			= Nothing
	, mbMenuGenFunc		= Nothing
	, taskFunc			= mkInteractiveTask`
	}	
where
	mkInteractiveTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTInteractiveTask taskInfo NoOutput}

mkInstantTask :: !d !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkInstantTask description taskfun =
	{ taskProperties	= {ManagerProperties|initManagerProperties & taskDescription = toDescr description}
	, groupedProperties	= initGroupedProperties
	, mbTaskNr			= Nothing
	, mbMenuGenFunc		= Nothing
	, taskFunc			= mkInstantTask`
	}
where
	mkInstantTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTFinishedTask taskInfo NoOutput} //We use a FinishedTask node because the task is finished after one evaluation

mkMonitorTask :: !d !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkMonitorTask description taskfun =
	{ taskProperties	= {ManagerProperties|initManagerProperties & taskDescription = toDescr description}
	, groupedProperties	= initGroupedProperties
	, mbTaskNr			= Nothing
	, mbMenuGenFunc		= Nothing
	, taskFunc			= mkMonitorTask`
	}
where
	mkMonitorTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTMonitorTask taskInfo NoOutput}

mkInstructionTask :: !d !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkInstructionTask description taskfun =
	{ taskProperties	= {ManagerProperties|initManagerProperties & taskDescription = toDescr description}
	, groupedProperties	= initGroupedProperties
	, mbTaskNr			= Nothing
	, mbMenuGenFunc		= Nothing
	, taskFunc			= mkInstructionTask`
	}
where
	mkInstructionTask` tst =:{TSt | taskInfo}
		= taskfun {tst & tree = TTInstructionTask taskInfo NoOutput}

mkRpcTask :: !d !RPCExecute !(String -> a) -> Task a | gUpdate{|*|} a & descr d
mkRpcTask description rpce parsefun =
	{ taskProperties	= {ManagerProperties|initManagerProperties & taskDescription = toDescr description}
	, groupedProperties	= initGroupedProperties
	, mbTaskNr			= Nothing
	, mbMenuGenFunc		= Nothing
	, taskFunc			= mkRpcTask`
	}
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
		
mkSequenceTask :: !d !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkSequenceTask description taskfun =
	{ taskProperties	= {ManagerProperties|initManagerProperties & taskDescription = toDescr description}
	, groupedProperties	= initGroupedProperties
	, mbTaskNr			= Nothing
	, mbMenuGenFunc		= Nothing
	, taskFunc			= mkSequenceTask`
	}
where
	mkSequenceTask` tst=:{TSt|taskNr,taskInfo}
		= taskfun {tst & tree = TTSequenceTask taskInfo [], taskNr = [0:taskNr]}
			
mkParallelTask :: !d !TaskParallelType !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkParallelTask description tpt taskfun =
	{ taskProperties	= {ManagerProperties|initManagerProperties & taskDescription = toDescr description}
	, groupedProperties	= initGroupedProperties
	, mbTaskNr			= Nothing
	, mbMenuGenFunc		= Nothing
	, taskFunc			= mkParallelTask`
	}
where
	mkParallelTask` tst=:{TSt|taskNr,taskInfo}
		# tst = {tst & tree = TTParallelTask taskInfo tpt []}												
		= taskfun tst

mkGroupedTask :: !d !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkGroupedTask description taskfun =
	{ taskProperties	= {ManagerProperties|initManagerProperties & taskDescription = toDescr description}
	, groupedProperties	= initGroupedProperties
	, mbTaskNr			= Nothing
	, mbMenuGenFunc		= Nothing
	, taskFunc			= mkGroupedTask`
	}
where
	mkGroupedTask` tst=:{TSt|taskInfo}
		# tst = {tst & tree = TTGroupedTask taskInfo [] [] Nothing}
		= taskfun tst
			
mkMainTask :: !d !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkMainTask description taskfun =
	{ taskProperties	= {ManagerProperties|initManagerProperties & taskDescription = toDescr description}
	, groupedProperties	= initGroupedProperties
	, mbTaskNr			= Nothing
	, mbMenuGenFunc		= Nothing
	, taskFunc			= mkMainTask`
	}
where
	mkMainTask` tst=:{taskNr,taskInfo}
		= taskfun {tst & tree = TTMainTask taskInfo initTaskProperties Nothing (TTFinishedTask taskInfo NoOutput)}

applyTask :: !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a
applyTask {taskProperties, groupedProperties, mbMenuGenFunc, mbTaskNr, taskFunc} tst=:{taskNr,tree,properties}
	# taskId								= iTaskId taskNr ""
	# (taskVal,tst)							= accIWorldTSt (loadValue taskId) tst
	# taskInfo =	{ TaskInfo
					| taskId				= taskNrToString taskNr
					, subject				= taskProperties.ManagerProperties.taskDescription.TaskDescription.title
					, description			= toString taskProperties.ManagerProperties.taskDescription.TaskDescription.description
					, context				= Nothing
					, tags					= taskProperties.ManagerProperties.tags
					, groupedBehaviour 		= groupedProperties.GroupedProperties.groupedBehaviour
					, groupActionsBehaviour	= groupedProperties.GroupedProperties.groupActionsBehaviour
					, menus					= case mbMenuGenFunc of
												Just f	= Just (GenFunc f)
												Nothing	= Nothing
					, formWidth				= taskProperties.ManagerProperties.formWidth
					}
	# tst = {TSt|tst & taskInfo = taskInfo, newTask = isNothing taskVal}
	= case taskVal of
		(Just (TaskFinished a))	
			# tst = addTaskNode (TTFinishedTask taskInfo (UIOutput (visualizeAsHtmlDisplay a))) tst
			= (TaskFinished a, {tst & taskNr = incTaskNr taskNr})
		_
			// If the task is new, but has run in a different context, initialize the states of the task and its subtasks
			# tst	= case (taskVal,mbTaskNr) of
						(Nothing, Just initTaskNr)	= copyTaskStates initTaskNr taskNr tst
						_							= tst
			// Execute task function
			# (result, tst=:{tree=node})			= taskFunc tst
			// Update task state
			= case result of
				(TaskFinished a)
					//If a process is finished (tl taskNr == procId), remove the process from the process DB (if it's allowed to be deleted)
					# procId				= taskNrToString (tl taskNr)
					# (gc,tst)				= if(procId == tst.TSt.properties.systemProperties.SystemProperties.taskId)
													(garbageCollectTaskInstance procId {TSt | tst & tree=node})
													(False,tst)
					//Garbage collect task store
					# tst					= deleteTaskStates taskNr tst					
					// Store final value if the process is not garbage collected
					# tst					= if (gc) tst (appIWorldTSt (storeValue taskId result) tst)
					# tst					= addTaskNode (TTFinishedTask taskInfo (UIOutput (visualizeAsHtmlDisplay a)))
												{tst & taskNr = incTaskNr taskNr, tree = tree}
					= (TaskFinished a, tst)
				(TaskBusy)
					// Store intermediate value
					# procId				= taskNrToString (tl taskNr)	
					# tst					= addTaskNode (finalizeTaskNode node)
												{tst & taskNr = incTaskNr taskNr, tree = tree}
					# tst					= appIWorldTSt (storeValue taskId result) tst
					= (TaskBusy, tst)
				(TaskException e)
					// Store exception
					# tst					= appIWorldTSt (storeValue taskId result) tst
					# tst					= addTaskNode (TTFinishedTask taskInfo (UIOutput (Text "Uncaught exception")))
												{tst & taskNr = incTaskNr taskNr, tree = tree}
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
	(TTMainTask ti mti inptype task)			= {tst & tree = TTMainTask ti mti inptype node} 			//Just replace the subtree 
	(TTSequenceTask ti tasks)					= {tst & tree = TTSequenceTask ti [node:tasks]}					//Add the node to the sequence
	(TTParallelTask ti tpi tasks)				= {tst & tree = TTParallelTask ti tpi [node:tasks]}				//Add the node to the parallel set
	(TTGroupedTask ti tasks gActions mbFocus)	= {tst & tree = TTGroupedTask ti [node:tasks] gActions mbFocus}	//Add the node to the grouped set
	_											= {tst & tree = tree}
		
setTUIDef :: ![TUIDef] ![(Action,Bool)] !*TSt -> *TSt
setTUIDef def accActions tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask info (UIOutput (Definition def accActions))}
		_								= tst

setTUIUpdates :: ![TUIUpdate] ![(Action,Bool)] !*TSt -> *TSt
setTUIUpdates upd actions tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask info (UIOutput (Updates upd actions))}
		_								= tst
		
setTUIFunc :: (*IWorld -> *(!InteractiveTask, !*IWorld)) !*TSt -> *TSt
setTUIFunc func tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask info (UIOutput (Func func))}
		_								= tst

setTUIMessage :: ![TUIDef] ![(Action,Bool)] !*TSt -> *TSt
setTUIMessage msg actions tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)		= {tst & tree = TTInteractiveTask info (UIOutput (Message msg actions))}
		_								= tst

setStatus :: !HtmlTag !*TSt -> *TSt
setStatus msg tst=:{tree}
	= case tree of
		(TTMonitorTask info _)				= {tst & tree = TTMonitorTask info (UIOutput msg)}
		_									= tst

setInstruction :: !(Maybe HtmlTag) !*TSt -> *TSt
setInstruction context tst=:{tree}
	= case tree of
			(TTInstructionTask ti _)	= {tst & tree = TTInstructionTask ti (UIOutput context)}
			_							= tst
			
setGroupActions :: ![(Action, (Either Bool (*IWorld -> *(!Bool,!*IWorld))))] !*TSt -> *TSt
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
		(TTInteractiveTask info _)				= {tst & tree = TTInteractiveTask info (JSONOutput (JSONValue json))}
		
setJSONFunc :: !(*IWorld -> *(!JSONNode,!*IWorld)) !*TSt -> *TSt
setJSONFunc f tst=:{tree}
	= case tree of
		(TTInteractiveTask info _)				= {tst & tree = TTInteractiveTask info (JSONOutput (JSONFunc f))}

/**
* Store and load the result of a workflow instance
*/
loadProcessResult :: !TaskNr !*TSt -> (!Maybe (TaskResult Dynamic), !*TSt)
loadProcessResult taskNr tst = accIWorldTSt (loadValue key) tst
where
	key = "iTask_"+++(taskNrToString taskNr)+++"-result"
	
storeProcessResult :: !TaskNr !(TaskResult Dynamic) !*TSt -> *TSt
storeProcessResult taskNr result tst
	// Only store process result if the process is not garbage collected (still exists in te process table)
	# (storeResult,tst) = storeResult tst
	= if (storeResult) (appIWorldTSt (storeValueAs SFDynamic key result) tst) tst
where
	key = "iTask_"+++(taskNrToString taskNr)+++"-result"
	
	storeResult tst 
		# (mbproc,tst) = getProcess (taskNrToString taskNr) tst
		= (isJust mbproc, tst) 

// Store the final value and it's type as a dynamic value, so it can be visualized by the task-result service later.		
storeProcessContainer :: !TaskId !(TaskResult Dynamic) !Dynamic !*TSt -> *TSt
storeProcessContainer taskId result thread tst
	= case (result,thread) of
		(TaskFinished (x :: a), (Container t) :: Container (TaskThread a) a )
			= storeContainer taskId x tst
		_
			= tst
where
	storeContainer :: !TaskId !a !*TSt -> *TSt | iTask a	
	storeContainer taskId value tst
		= appIWorldTSt (storeValueAs SFDynamic (taskId+++"-container") (dynamic (Container value) :: Container a^ a^)) tst

setTaskStore :: !String !a !*TSt -> *TSt | JSONEncode{|*|}, JSONDecode{|*|}, TC a
setTaskStore key value tst=:{taskNr}
	= appIWorldTSt (setTaskStoreFor taskNr key value) tst
	
setTaskStoreFor :: !TaskNr !String !a !*IWorld -> *IWorld | JSONEncode{|*|}, JSONDecode{|*|}, TC a
setTaskStoreFor taskNr key value iworld
	= storeValue (storekey taskNr key) value iworld

getTaskStore :: !String !*TSt -> (Maybe a, !*TSt) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
getTaskStore key tst=:{taskNr}
	= accIWorldTSt (getTaskStoreFor taskNr key) tst

getTaskStoreFor	:: !TaskNr !String !*IWorld -> (Maybe a, !*IWorld) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
getTaskStoreFor taskNr key iworld
	= loadValue (storekey taskNr key) iworld
	
getTaskStoreTimestamp :: !String !*TSt -> (Maybe Timestamp, !*TSt)
getTaskStoreTimestamp key tst=:{taskNr}
	= accIWorldTSt (getTaskStoreTimestampFor taskNr key) tst

getTaskStoreTimestampFor :: !TaskNr !String !*IWorld -> (Maybe Timestamp, !*IWorld)
getTaskStoreTimestampFor taskNr key iworld
	= getTimestamp (storekey taskNr key) iworld

storekey taskNr key= "iTask_" +++ (taskNrToString taskNr) +++ "-" +++ key

getEvents :: !*TSt -> ([(!String,!JSONNode)],!*TSt)
getEvents tst=:{taskNr,events} = (match,{TSt|tst & events = rest})
where
	(match,rest) = splitEvents events
	
	splitEvents [] = ([],[])
	splitEvents [event=:(t,n,v):events]
		= let (match,rest) = splitEvents events in
			if (t == taskId) ([(n,v):match],rest) (match, [event:rest])

	taskId = taskNrToString taskNr

resetSequence :: !*TSt -> *TSt
resetSequence tst=:{taskNr,tree}
	= case tree of
		(TTSequenceTask info sequence)	= {tst & taskNr = [0:tl taskNr], tree = TTSequenceTask info []}
		_								= {tst & tree = tree}

deleteTaskStates :: !TaskNr !*TSt -> *TSt
deleteTaskStates taskNr tst
	// Delete values in the data store
	= appIWorldTSt (deleteValues (iTaskId taskNr "")) tst
	
copyTaskStates :: !TaskNr !TaskNr !*TSt	-> *TSt
copyTaskStates fromtask totask tst
	// Copy values in the data store
	# tst 	= appIWorldTSt (copyValues (iTaskId fromtask "") (iTaskId totask "")) tst
	// Copy subprocess in the process table
	# tst	= copySubProcesses (taskNrToString fromtask) (taskNrToString totask) tst
	= tst
	
flushStore :: !*TSt -> *TSt
flushStore tst = appIWorldTSt flushCache tst

events2Paths :: ![(!String,!String)] -> [DataPath]
events2Paths updates = map (s2dp o fst) updates