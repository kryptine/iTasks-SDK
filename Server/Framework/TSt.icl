implementation module TSt

import StdList, StdTuple, StdBool, StdMisc, Maybe
import HTTP, Util, Text
import ProcessDB, SessionDB, ChangeDB, DocumentDB, UserDB, TaskTree
import GenEq, GenVisualize, GenUpdate, Store, Config, dynamic_string
import CoreCombinators, InteractionTasks
from StdFunc	import id, const, o
from JSON		import JSONDecode, fromJSON

ITERATION_THRESHOLD :== 10 // maximal number of allowed iterations during calculation of task tree

mkTSt :: !String !Config !HTTPRequest ![Workflow] !*Store !*World -> *TSt
mkTSt appName config request workflows store world
	=	{ taskNr			= []
		, taskInfo			= initTaskInfo
		, tree				= TTMainTask initTaskInfo initTaskProperties Nothing (TTFinishedTask initTaskInfo (noOutput 1))
		, newTask			= False
		, events			= []
		, properties		= initTaskProperties
		, staticInfo		= initStaticInfo appName workflows
		, currentChange		= Nothing
		, pendingChanges	= []
		, request			= request
		, iworld			= initIWorld appName config store world
		, sharedChanged		= False
		, triggerPresent	= False
		, iterationCount	= 1
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
	# (timestamp,world)	= time world
	# (dateTime,world)	= currentDateTimeWorld world
	= 	{ IWorld
		| application		= application
		, config			= config
		, store				= store
		, world				= world
		, timestamp			= timestamp
		, localDateTime		= dateTime
		}
		
initTaskInfo :: NonNormalizedTaskInfo
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
	, subTaskWorkers = []
	, deleteWhenDone = False
	}
	
initTaskProperties :: TaskProperties
initTaskProperties
	= { systemProperties	= initSystemProperties
	  , managerProperties	= initManagerProperties
	  , progress			= TPActive
	  }

initSession :: !SessionId !*TSt -> (!Maybe String, !*TSt)
initSession sessionId tst
	# (mbSession,timeout,tst=:{staticInfo})	= restoreSession sessionId tst
	= case mbSession of
		Nothing
			= (Just (if timeout "Your session timed out" "Failed to load session"), tst)
		Just session
			= (Nothing, {tst & staticInfo = {staticInfo & currentSession = session}})

createTaskInstance :: !Dynamic !Bool !(Maybe TaskParallelType) !Bool !Bool !*TSt -> (!ProcessId, !TaskResult Dynamic, !NonNormalizedTree, !*TSt)
createTaskInstance thread=:(_ :: Container (Container (TaskThreadParam a b) b) a) toplevel mbParType activate delete tst
	= createTaskInstance (toNonParamThreadEnter thread) toplevel mbParType activate delete tst

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
			, subTaskWorkers = []
			, deleteWhenDone = delete
			}
		, managerProperties	= managerProperties
		, progress 			= TPActive
		}
	# process =
		{ Process
		| taskId		 = taskId
		, properties	 = properties
		, dependents	 = []
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
		# (result,tree,tst)	= evaluateTaskInstance process [] Nothing toplevel True {tst & staticInfo = {tst.staticInfo & currentProcessId = processId}}
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
		= TTMainTask info properties mbParType (TTFinishedTask info noOutput)

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
createThread :: !(Task a) -> Dynamic | iTask a
createThread task = (dynamic container :: Container (TaskThread a^) a^)
where
 	container = Container {TaskThread|originalTask = task, currentTask = task}
 	
createThreadParam :: !(a -> Task b)	-> Dynamic | iTask a & iTask b
createThreadParam task = (dynamic container :: Container (Container (TaskThreadParam a^ b^) b^) a^)
where
 	container = Container (Container ({TaskThreadParam|originalTask = task, currentTask = task}))
 	
toNonParamThreadValue :: !String !Dynamic -> Maybe Dynamic
toNonParamThreadValue vStr (Container (Container {TaskThreadParam|originalTask,currentTask}) :: Container (Container (TaskThreadParam a b) b) a)
	= case fromJSON (fromString vStr) of
		Just v = 
			Just (dynamic Container {TaskThread | originalTask = originalTask v, currentTask = currentTask v} :: Container (TaskThread b) b)
		Nothing =
			Nothing
toNonParamThreadValue _ _ = Nothing

toNonParamThreadEnter :: !Dynamic -> Dynamic
toNonParamThreadEnter (Container (Container {TaskThreadParam|originalTask,currentTask}) :: Container (Container (TaskThreadParam a b) b) a)
	= (dynamic Container {TaskThread | originalTask = enterParam originalTask, currentTask = enterParam currentTask} :: Container (TaskThread b) b)
where		
	enterParam paramTask = enterInformation ("Workflow parameter","Enter the parameter of the workflow") >>= paramTask

applyThread :: !Dynamic !(Maybe TaskParallelType) !*TSt -> (!TaskResult Dynamic, !*TSt)
applyThread (Container {TaskThread|currentTask} :: Container (TaskThread a) a) inptype tst=:{taskNr}
	# (_,tst)		= applyTaskEdit currentTask tst
	# (result,tst)	= applyTaskCommit` tst
	= case result of
			TaskBusy		= (TaskBusy, tst)
			TaskFinished a	= (TaskFinished (dynamic a), tst)
			TaskException e	= (TaskException e, tst)
where
	applyTaskCommit` tst=:{TSt|iterationCount,properties=properties=:{TaskProperties|systemProperties=s=:{SystemProperties|taskId}}}
		// reset task nr
		# processTaskNr									= taskNrFromString taskId
		# tst											= {tst & taskNr = [taskNr !! (length taskNr - length processTaskNr - 1):processTaskNr]}
		// reset tree
		# info 											=	{ TaskInfo|initTaskInfo
															& taskId	= taskId
															, subject	= properties.managerProperties.ManagerProperties.taskDescription.TaskDescription.title
															}
		# tst											= {tst & tree = TTMainTask info properties inptype (TTFinishedTask info noOutput)}
		# (result, tst=:{sharedChanged,triggerPresent})	= applyTaskCommit currentTask {tst & sharedChanged = False, triggerPresent = False}
		| triggerPresent && sharedChanged && iterationCount < ITERATION_THRESHOLD
			= applyTaskCommit` {tst & iterationCount = inc iterationCount}
		| otherwise
			= (result,tst)
	
storeThread :: !ProcessId !Dynamic !*TSt -> *TSt
storeThread processId thread tst
	= appIWorldTSt (storeValueAs SFDynamic (iTaskId processId "thread") thread) tst

loadThread :: !ProcessId !*TSt -> (!Dynamic,!*TSt)
loadThread processId tst
	# (mbThread,tst) = accIWorldTSt (loadValue (iTaskId processId "thread")) tst
	= case mbThread of
		Just thread	= (thread,tst)
		Nothing		= abort ("Could not load task thread for process " +++ processId)
		
//END NEW THREAD FUNCTIONS

//Computes a workflow (sub) process
evaluateTaskInstance :: !Process ![TaskEvent] !(Maybe ChangeInjection) !Bool !Bool !*TSt-> (!TaskResult Dynamic, !NonNormalizedTree, !*TSt)
evaluateTaskInstance process=:{Process | taskId, properties, dependents, changeCount, inParallelType} events newChange isTop firstRun tst=:{TSt|currentChange,pendingChanges,tree=parentTree,events=parentEvents,properties=parentProperties,iworld=iworld=:{IWorld|timestamp=now}}
	// Update access timestamps in properties
	# properties						= {properties & systemProperties = {properties.systemProperties
															& latestEvent = Just now
															, firstEvent = case properties.systemProperties.firstEvent of
																Nothing	= Just now
																Just t	= Just t }}
	// Reset the task state
	# tst								= resetTSt taskId events properties inParallelType tst
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
											(applyAllChanges taskId changeCount pendingChanges thread properties inParallelType tst)
											(applyCurrentChange taskId changeCount thread properties inParallelType tst)
	// The tasktree of this process is the tree as it has been constructed, but with updated properties
	# (TTMainTask ti _ _ tasks,tst)		= getTaskTree tst
	# tree								= TTMainTask ti properties inParallelType tasks
	// Store the adapted persistent changes
	# tst								= if isTop (storePersistentChanges taskId tst) tst
	# tst								= restoreTSt parentTree parentEvents parentProperties tst
	= case result of
		TaskBusy
			//Update process table (changeCount & properties)
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Active}}
			# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, changeCount = changeCount}) tst
			= (TaskBusy, tree, tst)
		TaskFinished dyn
			//Store result
			# tst 			= appIWorldTSt (storeProcessResult (taskNrFromString taskId) result) tst
			//Store result container
			# tst			= storeProcessContainer taskId result thread tst
			//Update process table (status, changeCount & properties)
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Finished}}
			# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, changeCount = changeCount}) tst
			//Evaluate dependent processes
			# tst			= evaluateDependent dependents tst
			| isTop
				//Evaluate parent process (can probably be included in dependents evaluation)
				| isJust properties.systemProperties.parent
					# (mbParentProcess,tst) = getProcess (fromJust properties.systemProperties.parent) tst
					= case mbParentProcess of
						Nothing 
							= (result,tree,tst)
						Just parentProcess
							# (_,_,tst)	= evaluateTaskInstance parentProcess events Nothing True False tst
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
			# tst 		= appIWorldTSt (storeProcessResult (taskNrFromString taskId) result) tst
			//Update process table
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Excepted}}
			# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, changeCount = changeCount}) tst
			= (TaskException e, tree, tst)
where
	resetTSt :: !TaskId ![TaskEvent] !TaskProperties !(Maybe TaskParallelType) !*TSt -> *TSt
	resetTSt taskId events properties inptype tst
		# taskNr	= taskNrFromString taskId
		# info =	{ TaskInfo|initTaskInfo
					& taskId	= taskId
					, subject	= properties.managerProperties.ManagerProperties.taskDescription.TaskDescription.title
					}
		# tree		= TTMainTask info properties inptype (TTFinishedTask info noOutput)
		= {TSt| tst & taskNr = taskNr, tree = tree, events = events, staticInfo = {tst.staticInfo & currentProcessId = taskId}}	
	
	restoreTSt :: !NonNormalizedTree ![TaskEvent] !TaskProperties !*TSt -> *TSt
	restoreTSt tree events properties tst = {TSt|tst & tree = tree, events = events, properties = properties}
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
	applyAllChanges :: !ProcessId !Int [(!ChangeLifeTime,!Dynamic)] !Dynamic !TaskProperties !(Maybe TaskParallelType) !*TSt -> (!Int, !Dynamic, !TaskProperties, !TaskResult Dynamic, !*TSt)
	applyAllChanges processId changeCount [] thread properties inptype tst
		//Only apply the current change
		= applyCurrentChange processId changeCount thread properties inptype tst
	applyAllChanges processId changeCount changes thread properties inptype tst=:{currentChange}
		//Add the pending changes one after another (start empty)
		= applyAllChanges` processId changeCount currentChange changes thread properties {tst & pendingChanges = []} 
	where
		applyAllChanges` processId changeCount currentChange [] thread properties tst
			= applyCurrentChange processId changeCount thread properties inptype {tst & currentChange = currentChange}
		applyAllChanges` processId changeCount currentChange [c:cs] thread properties tst
			# (changeCount,thread,properties,result,tst) = applyCurrentChange processId changeCount thread properties inptype {tst & currentChange = Just c} 
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

	applyCurrentChange :: !ProcessId !Int !Dynamic !TaskProperties !(Maybe TaskParallelType) !*TSt -> (!Int, !Dynamic, !TaskProperties, !TaskResult Dynamic, !*TSt)
	applyCurrentChange processId changeCount thread properties inptype tst=:{currentChange}
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
					= applyThread thread inptype {TSt|tst & taskNr = [changeCount: taskNrFromString processId], properties = properties}
				= (changeCount,thread,properties,result,{TSt|tst & properties = properties})
			Nothing
				# (result,tst=:{TSt|properties})
					= applyThread thread inptype {TSt|tst & taskNr = [changeCount: taskNrFromString processId], properties = properties}
				= (changeCount,thread,properties,result,{TSt|tst & properties = properties})
	
	applyChange :: !TaskNr !Dynamic !Dynamic !TaskProperties -> (!Maybe Dynamic, !Maybe Dynamic, !TaskProperties)
	//Apply a change that matches a specific type
	applyChange cxt (changeFun :: Change a) (Container (thread=:{TaskThread|originalTask,currentTask}) :: Container (TaskThread a) a) properties
		# (mbProps,mbTask,mbChange) = changeFun properties (setTaskContext cxt currentTask) originalTask
		# newThread		= case mbTask of
								Just task	= Just (dynamic Container {TaskThread|thread & currentTask = task} :: Container (TaskThread a) a)
								Nothing		= Nothing
		# newProperties = case mbProps of
								Just props	= props
								Nothing		= properties
		= (mbChange,newThread,newProperties)
	//Apply a change that matches any type
	applyChange cxt (changeFun :: A.c: Change c | iTask c) (Container (thread=:{TaskThread|originalTask,currentTask}) :: Container (TaskThread a) a) properties
		# (mbProps,mbTask,mbChange) = changeFun properties (setTaskContext cxt currentTask) originalTask
		# newThread		= case mbTask of
								Just task	= Just (dynamic Container {TaskThread|thread & currentTask = task} :: Container (TaskThread a) a)
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

	evaluateDependent :: ![ProcessId] !*TSt -> *TSt
	evaluateDependent [] tst = tst
	evaluateDependent [p:ps] tst
		# (mbProc,tst)	= getProcess p tst
		= case mbProc of
			Nothing
				= evaluateDependent ps tst
			Just proc 
				# (_,_,tst)	= evaluateTaskInstance proc [] Nothing True False tst
				= evaluateDependent ps tst

applyChangeToTaskTree :: !ProcessId !ChangeInjection !*TSt -> *TSt
applyChangeToTaskTree pid (lifetime,change) tst=:{taskNr,taskInfo,tree,staticInfo,currentChange,pendingChanges, properties}
	# (mbProcess,tst) = getProcess pid tst
	= case mbProcess of
		(Just proc) 
			# (_,_,tst) = evaluateTaskInstance proc [] (Just (lifetime,change)) True False tst
			= {tst & taskNr = taskNr, taskInfo = taskInfo,properties = properties
			  , tree = tree, staticInfo = staticInfo, currentChange = currentChange, pendingChanges = pendingChanges}
		Nothing		
			= tst

calculateTaskTree :: !TaskId ![TaskEvent] !*TSt -> (!NonNormalizedTree, !*TSt)
calculateTaskTree taskId events tst
	# (mbProcess,tst) = getProcess taskId tst
	= case mbProcess of
		Nothing
			# info =	{ TaskInfo | initTaskInfo
						& taskId			= taskId
						, subject			= "Deleted Process"
						, description		= "Task Result"
						}
			= (TTFinishedTask info noProcessResult, tst)
		Just process=:{Process|properties}
			= case properties.systemProperties.SystemProperties.status of
				Active
					//Evaluate the process
					# (result,tree,tst) = evaluateTaskInstance process events Nothing True False tst
					= (tree,tst)
				_		
					//retrieve process result from store and show it??
					# (mbContainer,tst) = accIWorldTSt (loadValue (taskId +++ "-container")) tst				
					# result = case mbContainer of
						(Just dyn)	= (renderResult dyn,jsonResult dyn)
						(Nothing)	= noProcessResult
					# info =	{ TaskInfo| initTaskInfo
								& taskId			= taskId
								, subject			= properties.managerProperties.ManagerProperties.taskDescription.TaskDescription.title
								, description		= "Task Result"
								}
					= (TTFinishedTask info result,tst)

renderResult :: !Dynamic -> HtmlTag
renderResult (Container value :: Container a a) = visualizeAsHtmlDisplay value

jsonResult :: !Dynamic -> JSONNode
jsonResult (Container value :: Container a a) = toJSON value

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

getTaskTree :: !*TSt	-> (!NonNormalizedTree, !*TSt)
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

mapTaskFunctions :: !(a -> b) !(TaskFunctions a) -> TaskFunctions b
mapTaskFunctions f funcs = appSnd ((o) (appFst (mapTaskResult f))) funcs

mkTaskFunction :: (*TSt -> (!a,!*TSt)) -> TaskFunctionCommit a
mkTaskFunction f = \tst -> let (a,tst`) = f tst in (TaskFinished a,tst`)
		
mkInteractiveTask :: !d !InteractiveTaskType !(TaskFunctions a) -> Task a | descr d
mkInteractiveTask description type (taskfunE,taskfunC)
	= mkTask
		description
		taskfunE
		(\tst=:{taskInfo} -> taskfunC {tst & tree = TTInteractiveTask taskInfo type noOutput})

mkInstantTask :: !d !(TaskFunctionCommit a) -> Task a | descr d
mkInstantTask description taskfun
	= mkTask
		description
		id
		(\tst=:{taskInfo} -> taskfun {tst & tree = TTFinishedTask taskInfo noOutput}) //We use a FinishedTask node because the task is finished after one evaluation
		
mkSequenceTask :: !d !(TaskFunctions a) -> Task a | descr d
mkSequenceTask description (taskfunE,taskfunC)
	= mkTask
		description 
		(\tst=:{taskNr}					-> taskfunE {tst & taskNr = [0:taskNr]})
		(\tst=:{TSt|taskNr,taskInfo}	-> taskfunC {tst & taskNr = [0:taskNr], tree = TTSequenceTask taskInfo []})
			
mkParallelTask :: !d !TaskParallelType !(TaskFunctions a) -> Task a | descr d
mkParallelTask description tpt (taskfunE,taskfunC)
	= mkTask
		description
		taskfunE
		(\tst=:{taskInfo} -> taskfunC {tst & tree = TTParallelTask taskInfo tpt []})

mkGroupedTask :: !d !(TaskFunctions a) -> Task a | descr d
mkGroupedTask description (taskfunE,taskfunC)
	= mkTask
		description
		taskfunE
		(\tst=:{taskInfo,taskNr} -> taskfunC {tst & tree = TTGroupedTask taskInfo [] [] Nothing})
			
mkMainTask :: !d !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkMainTask description taskfun
	= mkTask
		description
		id
		(\tst=:{taskInfo} -> taskfun {tst & tree = TTMainTask taskInfo initTaskProperties Nothing (TTFinishedTask taskInfo noOutput)})

mkTask :: !d !(*TSt -> *TSt) !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkTask description taskFuncEdit taskFuncCommit =
	{ taskProperties	= {ManagerProperties|initManagerProperties & taskDescription = toDescr description}
	, groupedProperties	= initGroupedProperties
	, formWidth			= Nothing
	, mbTaskNr			= Nothing
	, mbMenuGenFunc		= Nothing
	, taskFuncEdit		= taskFuncEdit
	, taskFuncCommit	= taskFuncCommit
	}

applyTaskEdit :: !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a
applyTaskEdit {taskFuncEdit,mbTaskNr} tst=:{taskNr}
	# taskId								= iTaskId taskNr ""
	# (taskVal,tst)							= accIWorldTSt (loadValue taskId) tst
	= case (taskVal,mbTaskNr) of
		(Just (TaskFinished a),_)
			= (TaskFinished a, {tst & taskNr = incTaskNr taskNr})
		(Just _,_)
			// Execute task function
			# tst = taskFuncEdit tst
			= (TaskBusy,tst)
		(Nothing, Just initTaskNr)
			// If the task is new, but has run in a different context, initialize the states of the task and its subtasks
			# tst = copyTaskStates initTaskNr taskNr tst
			// Execute task function
			# tst = taskFuncEdit tst
			= (TaskBusy,tst)
		(Nothing,Nothing)
			// no edit pass for new task
			= (TaskBusy,tst)

applyTaskCommit :: !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a
applyTaskCommit {taskProperties, groupedProperties, mbMenuGenFunc, mbTaskNr, taskFuncCommit, formWidth} tst=:{taskNr,tree,properties}
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
					, menus					= mbMenuGenFunc
					, formWidth				= formWidth
					}
	# tst = {TSt|tst & taskInfo = taskInfo, newTask = isNothing taskVal}
	= case taskVal of
		(Just (TaskFinished a))	
			# tst = addTaskNode (TTFinishedTask taskInfo (visualizeAsHtmlDisplay a,toJSON a)) tst
			= (TaskFinished a, {tst & taskNr = incTaskNr taskNr})
		_
			// Execute task function
			# (result, tst=:{tree=node})	= taskFuncCommit tst
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
					# tst					= addTaskNode (TTFinishedTask taskInfo (visualizeAsHtmlDisplay a,toJSON a))
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
					# tst					= addTaskNode (TTFinishedTask taskInfo (finishedStrOutput "Uncaught exception"))
												{tst & taskNr = incTaskNr taskNr, tree = tree}
					= (TaskException e, tst)
where
	//Perform reversal of lists that have been accumulated in reversed order
	finalizeTaskNode (TTSequenceTask ti tasks) 					= TTSequenceTask	ti (reverse tasks)
	finalizeTaskNode (TTParallelTask ti tpi tasks)				= TTParallelTask	ti tpi (reverse tasks)
	finalizeTaskNode (TTGroupedTask ti tasks gActions mbFocus)	= TTGroupedTask		ti (reverse tasks) gActions mbFocus
	finalizeTaskNode node										= node

//Add a new node to the current sequence or process
addTaskNode :: !NonNormalizedTree !*TSt -> *TSt
addTaskNode node tst=:{tree} = case tree of
	TTMainTask ti mti inptype task			= {tst & tree = TTMainTask ti mti inptype node} 				//Just replace the subtree 
	TTSequenceTask ti tasks					= {tst & tree = TTSequenceTask ti [node:tasks]}					//Add the node to the sequence
	TTParallelTask ti tpi tasks				= {tst & tree = TTParallelTask ti tpi [node:tasks]}				//Add the node to the parallel set
	TTGroupedTask ti tasks gActions mbFocus	= {tst & tree = TTGroupedTask ti [node:tasks] gActions mbFocus}	//Add the node to the grouped set
	_										= {tst & tree = tree}

setInteractiveFuncs	:: !TTNNInteractiveTask !*TSt -> *TSt
setInteractiveFuncs funcs tst=:{tree}
	= case tree of
		TTInteractiveTask info type _		= {tst & tree = TTInteractiveTask info type funcs}
		_									= tst

setGroupActions	 :: ![(!Action,*IWorld -> *(!Bool,!*IWorld))] !*TSt -> *TSt
setGroupActions actions tst=:{tree}
	= case tree of
		TTGroupedTask info tasks _ tag		= {tst & tree = TTGroupedTask info tasks actions tag}
		_									= tst
		
setFocusCommand :: !String !*TSt -> *TSt
setFocusCommand tag tst=:{tree}
	= case tree of
		TTGroupedTask info tasks actions _	= {tst & tree = TTGroupedTask info tasks actions (Just tag)}
		_									= tst

/**
* Store and load the result of a workflow instance
*/
loadProcessResult :: !TaskNr !*IWorld -> (!Maybe (TaskResult Dynamic), !*IWorld)
loadProcessResult taskNr iworld = loadValue key iworld
where
	key = iTaskId taskNr "result"
	
storeProcessResult :: !TaskNr !(TaskResult Dynamic) !*IWorld -> *IWorld
storeProcessResult taskNr result iworld
	// Only store process result if the process is not garbage collected (still exists in te process table)
	# (storeResult,iworld) = storeResult iworld
	= if (storeResult) (storeValueAs SFDynamic key result iworld) iworld
where
	key = iTaskId taskNr "result"
	
	storeResult iworld 
		# (mbproc,iworld) = getProcess (taskNrToString taskNr) iworld
		= (isJust mbproc, iworld) 

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
	= storeValue (iTaskId taskNr key) value iworld

getTaskStore :: !String !*TSt -> (Maybe a, !*TSt) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
getTaskStore key tst=:{taskNr}
	= accIWorldTSt (getTaskStoreFor taskNr key) tst

getTaskStoreFor	:: !TaskNr !String !*IWorld -> (Maybe a, !*IWorld) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
getTaskStoreFor taskNr key iworld
	= loadValue (iTaskId taskNr key) iworld
	
getTaskStoreTimestamp :: !String !*TSt -> (Maybe Timestamp, !*TSt)
getTaskStoreTimestamp key tst=:{taskNr}
	= accIWorldTSt (getTaskStoreTimestampFor taskNr key) tst

getTaskStoreTimestampFor :: !TaskNr !String !*IWorld -> (Maybe Timestamp, !*IWorld)
getTaskStoreTimestampFor taskNr key iworld
	= getStoreTimestamp (iTaskId taskNr key) iworld
	
deleteTaskStore :: !String !*TSt -> *TSt
deleteTaskStore key tst=:{taskNr}
	= appIWorldTSt (deleteTaskStoreFor taskNr key) tst

deleteTaskStoreFor :: !TaskNr !String !*IWorld -> *IWorld
deleteTaskStoreFor taskNr key iworld
	= deleteValue (iTaskId taskNr key) iworld

//Get edit events for current task of which the name is a datapath
getEditEvents :: !*TSt -> (![(!DataPath,!String)],!*TSt)
getEditEvents tst
	# (events,tst) = getEvents isdps tst
	= ([(s2dp name,value) \\ (name,JSONString value) <- events],tst)

//Get value event for current task if present
getValueEvent :: !*TSt -> (!Maybe a,!*TSt) | JSONDecode{|*|} a
getValueEvent tst
	# (events,tst) = getEvents ((==) "value") tst
	= (maybe Nothing (fromJSON o snd) (listToMaybe events),tst)

//Get action event for current task if present
getActionEvent :: !*TSt -> (!Maybe JSONNode,!*TSt)
getActionEvent tst
	# (events,tst) = getEvents ((==) "action") tst	
	= (fmap snd (listToMaybe events),tst)

getEvents :: !(String -> Bool) !*TSt -> ([(!String,!JSONNode)],!*TSt)
getEvents pred tst=:{taskNr,events} = (match,{TSt|tst & events = rest})
where
	(match,rest) = splitEvents events
	
	splitEvents [] = ([],[])
	splitEvents [event=:(t,n,v):events]
		= let (match,rest) = splitEvents events in
			if (t == taskId && pred n) ([(n,v):match],rest) (match, [event:rest])

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

noOutput 				= abort "Task tree node without output."
noProcessResult 		= finishedStrOutput "Cannot load result."
finishedStrOutput str	= (Text str,JSONString str)
