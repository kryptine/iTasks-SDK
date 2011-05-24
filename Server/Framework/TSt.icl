implementation module TSt

import StdList, StdTuple, StdBool, StdMisc, Maybe, StdFile 
import Directory, Error, File, OSError
import HTTP, Util, Text
import ProcessDB, ChangeDB, DocumentDB, TaskTree, SessionDB
import GenEq, GenVisualize, GenUpdate, Store, Config
from StdFunc			import id, const, o, seq
from iTasks				import JSONDecode, fromJSON
from CoreCombinators	import >>=
from InteractionTasks	import enterInformation
from TuningCombinators	import @>>, <<@, class tune, instance tune Title, :: Title(..)

ITERATION_THRESHOLD :== 10 // maximal number of allowed iterations during calculation of task tree

mkTSt :: !String !Config ![Workflow] !Store !FilePath !*World -> *TSt
mkTSt appName config workflows store tmpDir world
	=	{ taskNr			= []
		, taskInfo			= initTaskInfo
		, tree				= initTree initTaskInfo
		, newTask			= False
		, editEvent			= Nothing
		, commitEvent		= Nothing
		, properties		= initProcessProperties
		, staticInfo		= initStaticInfo
		, currentChange		= Nothing
		, pendingChanges	= []
		, iworld			= initIWorld appName config store tmpDir workflows world
		, sharedChanged		= False
		, sharedDeleted		= False
		, iterationCount	= 1
		, interactionLayout	= defaultInteractionLayout
		, parallelLayout	= defaultParallelLayout
		, resultLayout		= defaultResultLayout
		}

initStaticInfo :: StaticInfo
initStaticInfo
	=	{ currentProcessId	= ""
		, currentSession 	= {Session | sessionId = "", user = AnyUser, timestamp = Timestamp 0}
		}

initIWorld	:: !String !Config !Store !FilePath ![Workflow] !*World -> *IWorld
initIWorld application config store tmpDir workflows world
	# (timestamp,world)	= time world
	# (dateTime,world)	= currentDateTimeWorld world
	= 	{ IWorld
		| application		= application
		, config			= config
		, store				= store
		, world				= world
		, timestamp			= timestamp
		, localDateTime		= dateTime
		, tmpDirectory		= tmpDir
		, currentUser		= AnyUser
		, staticWorkflows	= workflows
		}
		
initTaskInfo :: TaskInfo
initTaskInfo
	=	{ TaskInfo
		| taskId			= ""
		, title				= ""
		, description		= ""
		, type				= Nothing
		, isControlTask		= False
		, localInteraction	= False
		, interactionLayout	= TIInteractionLayoutMerger	defaultInteractionLayout
		, parallelLayout	= TIParallelLayoutMerger	defaultParallelLayout
		, resultLayout		= TIResultLayoutMerger		defaultResultLayout
		}

initSystemProperties :: SystemProperties
initSystemProperties =
	{SystemProperties
	| taskId = ""
	, parent = Nothing
	, status = Running
	, issuedAt = Timestamp 0
	, firstEvent = Nothing
	, latestEvent = Nothing
	, deleteWhenDone = False
	, menu = noMenu
	}
	
initProcessProperties :: ProcessProperties
initProcessProperties =
	{ taskProperties	= initTaskProperties
	, systemProperties	= initSystemProperties
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

createTaskInstance :: !Dynamic !Bool !Bool !ManagerProperties !ActionMenu !*TSt -> (!ProcessId, !TaskResult Dynamic, !NonNormalizedTree, !*TSt)
createTaskInstance thread=:(_ :: Container (Container (TaskThreadParam a b) b) a) toplevel delete managerProperties menu tst
	= createTaskInstance (toNonParamThreadEnter thread) toplevel delete managerProperties menu tst

createTaskInstance thread=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) toplevel delete managerProperties=:{worker} menu tst=:{taskNr,properties,iworld=iworld=:{IWorld|timestamp=currentTime}}
	# (worker,tst) 			= if (worker <> AnyUser) (worker,tst) (getCurrentUser tst)
	# taskId				= if toplevel "" (taskNrToString taskNr)
	# parent				= if toplevel Nothing (Just properties.systemProperties.SystemProperties.taskId)
	# managerProperties		= {managerProperties & worker = worker}
	# properties =
		{ taskProperties	= taskProperties originalTask
		, systemProperties =
			{ taskId			= taskId
			, parent			= parent
			, status			= Running
			, issuedAt			= currentTime
			, firstEvent		= Nothing
			, latestEvent		= Nothing
			, deleteWhenDone	= delete
			, menu				= menu
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
		}
	//Create an entry in the process table
	# (processId, tst)		= createProcess process tst
	//Load process as it is in the store
	# (Just process,tst)	= getProcess processId tst
	//Create a thread with task functions in the store
	# tst					= storeThread processId thread tst
	= case managerProperties.ManagerProperties.status of
		Active
			//If directly active, evaluate the process once to kickstart automated steps that can be set in motion immediately	
			# (result,tree,tst)	= evaluateTaskInstance process Nothing toplevel True {tst & staticInfo = {tst.staticInfo & currentProcessId = processId}}
			= (processId,result,tree,tst)
		Suspended
			= (processId, TaskBusy, node properties processId, tst)
where
	setUser worker props=:{worker=AnyUser} = {props & worker = worker}
	setUser worker props = props

	node properties taskId
		# taskNr	= taskNrFromString taskId
		# info =	{ TaskInfo|initTaskInfo
					& taskId	= taskId
					, title		= properties.ProcessProperties.taskProperties.TaskProperties.taskDescription.TaskDescription.title
					}
		= initTree info

deleteTaskInstance :: !ProcessId !*TSt -> *TSt
deleteTaskInstance procId tst 
	# (_,tst) 	= deleteProcess procId tst
	# tst		= deleteSubProcesses procId tst
	# tst		= appIWorldTSt (deleteValues procId) tst
	# tst		= deleteTaskStates (taskNrFromString procId) tst
	= tst

garbageCollectTaskInstance :: !ProcessId !*TSt -> (!Bool,!*TSt)
garbageCollectTaskInstance procId tst
	| tst.TSt.properties.systemProperties.deleteWhenDone
	# tst = deleteTaskInstance procId tst
	= (True,tst)	
	| otherwise
	= (False,tst)

//NEW THREAD FUNCTIONS
createThread :: (Task a) -> Dynamic | iTask a
createThread task = (dynamic container :: Container (TaskThread a^) a^)
where
 	container = Container {TaskThread|originalTask = task, currentTask = task}
 	
createThreadParam :: !String (a -> Task b)	-> Dynamic | iTask a & iTask b
createThreadParam title task = (dynamic container :: Container (Container (TaskThreadParam a^ b^) b^) a^)
where
 	container = Container (Container ({TaskThreadParam|originalTask = task, currentTask = task, title = title}))
 	
toNonParamThreadValue :: !String !Dynamic -> Maybe Dynamic
toNonParamThreadValue vStr (Container (Container {TaskThreadParam|originalTask,currentTask,title}) :: Container (Container (TaskThreadParam a b) b) a)
	= case fromJSON (fromString vStr) of
		Just v = 
			Just (dynamic Container {TaskThread | originalTask = originalTask v <<@ Title title, currentTask = currentTask v <<@ Title title} :: Container (TaskThread b) b)
		Nothing =
			Nothing
toNonParamThreadValue _ _ = Nothing

toNonParamThreadEnter :: !Dynamic -> Dynamic
toNonParamThreadEnter (Container (Container {TaskThreadParam|originalTask,currentTask,title}) :: Container (Container (TaskThreadParam a b) b) a)
	= (dynamic Container {TaskThread | originalTask = enterParam originalTask, currentTask = enterParam currentTask} :: Container (TaskThread b) b)
where		
	enterParam paramTask = Title title @>> (enterInformation ("Workflow parameter","Enter the parameter of the workflow") >>= paramTask)

applyThread :: !Dynamic !*TSt -> (!TaskResult Dynamic, !*TSt)
applyThread (Container {TaskThread|currentTask} :: Container (TaskThread a) a) tst=:{taskNr}
	# (_,tst)		= applyTaskEdit currentTask tst
	# (result,tst)	= applyTaskCommit` tst
	= case result of
			TaskBusy			= (TaskBusy, tst)
			TaskFinished a		= (TaskFinished (dynamic a), tst)
			TaskException e	str	= (TaskException e str, tst)
where
	applyTaskCommit` tst=:{TSt|iterationCount,properties=properties=:{ProcessProperties|systemProperties=s=:{SystemProperties|taskId}}}
		// reset task nr
		# processTaskNr									= taskNrFromString taskId
		# tst											= {tst & taskNr = [taskNr !! (length taskNr - length processTaskNr - 1):processTaskNr]}
		// reset tree
		# info 											=	{ TaskInfo|initTaskInfo
															& taskId	= taskId
															, title		= properties.ProcessProperties.taskProperties.TaskProperties.taskDescription.TaskDescription.title
															}
		# tst											= {tst & tree = initTree info}
		# (result, tst=:{sharedChanged,sharedDeleted})	= applyTaskCommit currentTask Nothing {tst & sharedChanged = False, sharedDeleted = False}
		| (sharedChanged || sharedDeleted) && iterationCount < ITERATION_THRESHOLD
			= applyTaskCommit` {tst & iterationCount = inc iterationCount}
		| otherwise
			= (result,tst)
	
storeThread :: !ProcessId !Dynamic !*TSt -> *TSt
storeThread processId thread tst
	= appIWorldTSt (storeValue (iTaskId processId "thread") thread) tst

loadThread :: !ProcessId !*TSt -> (!Dynamic,!*TSt)
loadThread processId tst
	# (mbThread,tst) = accIWorldTSt (loadValue (iTaskId processId "thread")) tst
	= case mbThread of
		Just thread	= (thread,tst)
		Nothing		= abort ("Could not load task thread for process " +++ processId)
		
//END NEW THREAD FUNCTIONS

//Computes a workflow (sub) process
evaluateTaskInstance :: !Process !(Maybe ChangeInjection) !Bool !Bool !*TSt-> (!TaskResult Dynamic, !NonNormalizedTree, !*TSt)
evaluateTaskInstance process=:{Process | taskId, properties, dependents, changeCount} newChange isTop firstRun tst=:{TSt|currentChange,pendingChanges,tree=parentTree,properties=parentProperties,iworld=iworld=:{IWorld|timestamp=now}}
	// Update access timestamps in properties
	# properties						= {properties & systemProperties = {properties.systemProperties
															& latestEvent = Just now
															, firstEvent = case properties.systemProperties.firstEvent of
																Nothing	= Just now
																Just t	= Just t }}
	// Reset the task state
	# tst								= resetTSt taskId properties tst
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
	# (tree,tst)						= getTaskTree tst
	// Store the adapted persistent changes
	# tst								= if isTop (storePersistentChanges taskId tst) tst
	# tst								= restoreTSt parentTree parentProperties tst
	= case result of
		TaskBusy
			//Update process table (changeCount & properties)
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Running}}
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
							# (_,_,tst)	= evaluateTaskInstance parentProcess Nothing True False tst
							= (result,tree,tst)	
				| otherwise
					= (result,tree,tst)
			| otherwise
				//Update process table (changeCount & properties)
				# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Finished}}
				# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, changeCount = changeCount}) tst
				= (result,tree,tst)
		TaskException e str
			//Store exception
			# tst 		= appIWorldTSt (storeProcessResult (taskNrFromString taskId) result) tst
			//Update process table
			# properties	= {properties & systemProperties = {SystemProperties|properties.systemProperties & status = Excepted}}
			# (_,tst)		= updateProcess taskId (\p -> {Process|p & properties = properties, changeCount = changeCount}) tst
			= (TaskException e str, tree, tst)
where
	resetTSt :: !TaskId !ProcessProperties !*TSt -> *TSt
	resetTSt taskId properties tst=:{TSt|iworld}
		# taskNr	= taskNrFromString taskId
		# info =	{ TaskInfo|initTaskInfo
					& taskId	= taskId
					, title		= properties.ProcessProperties.taskProperties.TaskProperties.taskDescription.TaskDescription.title
					}
		# tree		= initTree info
		# iworld	= {iworld & currentUser = properties.ProcessProperties.managerProperties.worker}
		= {TSt| tst & taskNr = taskNr, tree = tree, staticInfo = {tst.staticInfo & currentProcessId = taskId}, iworld = iworld}	
	
	restoreTSt :: !NonNormalizedTree !ProcessProperties !*TSt -> *TSt
	restoreTSt tree properties tst = {TSt|tst & tree = tree, properties = properties}
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
	applyAllChanges :: !ProcessId !Int [(!ChangeLifeTime,!Dynamic)] !Dynamic !ProcessProperties !*TSt -> (!Int, !Dynamic, !ProcessProperties, !TaskResult Dynamic, !*TSt)
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
				TaskException _ _
					//A change caused an exception. Stop, but keep pending changes
					= (changeCount,thread,properties,result,{tst & pendingChanges = tst.pendingChanges ++ cs, currentChange = currentChange})

	applyCurrentChange :: !ProcessId !Int !Dynamic !ProcessProperties !*TSt -> (!Int, !Dynamic, !ProcessProperties, !TaskResult Dynamic, !*TSt)
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
	
	applyChange :: !TaskNr !Dynamic !Dynamic !ProcessProperties -> (!Maybe Dynamic, !Maybe Dynamic, !ProcessProperties)
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
			# tst = appIWorldTSt (storeValue ("iTask_change-" +++ label) dyn) tst
			= storeChanges cs tst

	evaluateDependent :: ![ProcessId] !*TSt -> *TSt
	evaluateDependent [] tst = tst
	evaluateDependent [p:ps] tst
		# (mbProc,tst)	= getProcess p tst
		= case mbProc of
			Nothing
				= evaluateDependent ps tst
			Just proc 
				# (_,_,tst)	= evaluateTaskInstance proc Nothing True False tst
				= evaluateDependent ps tst

applyChangeToTaskTree :: !ProcessId !ChangeInjection !*TSt -> *TSt
applyChangeToTaskTree pid (lifetime,change) tst=:{taskNr,taskInfo,tree,staticInfo,currentChange,pendingChanges, properties}
	# (mbProcess,tst) = getProcess pid tst
	= case mbProcess of
		Just proc
			# (_,_,tst) = evaluateTaskInstance proc (Just (lifetime,change)) True False tst
			= {tst & taskNr = taskNr, taskInfo = taskInfo,properties = properties
			  , tree = tree, staticInfo = staticInfo, currentChange = currentChange, pendingChanges = pendingChanges}
		Nothing		
			= tst

calculateTaskTreeContainer :: !TaskId !*TSt -> (!NonNormalizedTreeContainer, !*TSt)
calculateTaskTreeContainer taskId tst
	# (mbProcess,tst) = getProcess taskId tst
	= case mbProcess of
		Nothing
			# info =	{ TaskInfo | initTaskInfo
						& taskId			= taskId
						, title				= "Deleted Process"
						, description		= "Task Result"
						}
			= (TTContainer noMenu (TTFinishedTask info noProcessResult False), tst)
		Just process=:{Process|properties}
			# (tree, tst) = case (properties.systemProperties.SystemProperties.status,properties.ProcessProperties.managerProperties.ManagerProperties.status) of
				(Running,Active)
					//Evaluate the process
					# (result,tree,tst) = evaluateTaskInstance process Nothing True False tst
					= (tree,tst)
				(Excepted,_)
					// show exception
					# (mbResult,tst) = accIWorldTSt (loadProcessResult (taskNrFromString taskId)) tst
					# output = case mbResult of
						Just (TaskException _ err)	= renderException err
						Nothing						= renderException ""
					# info =	{ TaskInfo| initTaskInfo
								& taskId			= taskId
								, title				= properties.ProcessProperties.taskProperties.TaskProperties.taskDescription.TaskDescription.title
								, description		= properties.ProcessProperties.taskProperties.TaskProperties.taskDescription.TaskDescription.description
								}
					= (TTFinishedTask info output True,tst)
				_		
					//retrieve process result from store and show it??
					# (mbContainer,tst) = accIWorldTSt (loadValue (taskId +++ "-container")) tst				
					# result = case mbContainer of
						Just dyn	= (renderResult dyn,jsonResult dyn)
						Nothing		= noProcessResult
					# info =	{ TaskInfo| initTaskInfo
								& taskId			= taskId
								, title				= properties.ProcessProperties.taskProperties.TaskProperties.taskDescription.TaskDescription.title
								, description		= properties.ProcessProperties.taskProperties.TaskProperties.taskDescription.TaskDescription.description
								}
					= (TTFinishedTask info result False,tst)
			= (TTContainer properties.systemProperties.SystemProperties.menu tree,tst)

renderResult :: !Dynamic -> HtmlTag
renderResult (Container value :: Container a a) = visualizeAsHtmlDisplay value

jsonResult :: !Dynamic -> JSONNode
jsonResult (Container value :: Container a a) = toJSON value

renderException :: !String -> TTNNFinished
renderException err = (html [H1Tag [] [SpanTag [StyleAttr "color: red"] [Text "Uncaught exception"]], BrTag [], Text err],JSONString ("Uncaught exception: " +++ err))

getCurrentSession :: !*TSt 	-> (!Session, !*TSt)
getCurrentSession tst =:{staticInfo} = (staticInfo.currentSession, tst)

getCurrentUser :: !*TSt -> (!User, !*TSt)
getCurrentUser tst =:{staticInfo}
	= (staticInfo.currentSession.user, {TSt | tst & staticInfo = staticInfo})

getCurrentProcess :: !*TSt -> (!ProcessId, !*TSt)
getCurrentProcess tst =: {staticInfo}
	= (staticInfo.currentProcessId, {tst & staticInfo = staticInfo})

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
getWorkflows tst=:{TSt|iworld=iworld =:{staticWorkflows}}
	= (staticWorkflows,tst)

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

mkTaskFunction :: (*TSt -> (!a,!*TSt)) -> TaskFunctionCommit a
mkTaskFunction f = \tst -> let (a,tst`) = f tst in (TaskFinished a,tst`)
		
mkInteractionTask :: !d !(TaskFunctions a) -> Task a | descr d
mkInteractionTask description (taskfunE,taskfunC)
	= mkTask
		description
		taskfunE
		(\tst=:{taskInfo} -> taskfunC {tst & tree = TTInteractionTask taskInfo noOutput})

mkInstantTask :: !d !(TaskFunctionCommit a) -> Task a | descr d
mkInstantTask description taskfun
	= mkTask
		description
		id
		(\tst=:{taskInfo} -> taskfun {tst & tree = initTree taskInfo}) //We use a FinishedTask node because the task is finished after one evaluation
		
mkSequenceTask :: !d !(TaskFunctions a) -> Task a | descr d
mkSequenceTask description (taskfunE,taskfunC)
	= mkTask
		description 
		(\tst=:{taskNr}					-> taskfunE {tst & taskNr = [0:taskNr]})
		(\tst=:{TSt|taskNr,taskInfo}	-> taskfunC {tst & taskNr = [0:taskNr], tree = initTree taskInfo})
			
mkParallelTask :: !d !(TaskFunctions a) -> Task a | descr d
mkParallelTask description (taskfunE,taskfunC)
	= mkTask
		description
		taskfunE
		(\tst=:{taskInfo} -> taskfunC {tst & tree = TTParallelTask taskInfo []})

mkTask :: !d !(*TSt -> *TSt) !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a | descr d
mkTask description taskFuncEdit taskFuncCommit =
	{ properties			= {TaskProperties|initTaskProperties & taskDescription = toDescr description}
	, mbTaskNr				= Nothing
	, taskFuncEdit			= taskFuncEdit
	, taskFuncCommit		= taskFuncCommit
	, mbInteractionLayout	= Nothing
	, mbParallelLayout		= Nothing
	, mbResultLayout		= Nothing
	}

applyTaskEdit :: !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a
applyTaskEdit {taskFuncEdit,mbTaskNr} tst=:{taskNr}
	# (mbTaskVal,tst) = accIWorldTSt (loadValue (iTaskId taskNr "")) tst
	= case mbTaskVal of
		Just (TaskFinished a)
			= (TaskFinished a, {tst & taskNr = incTaskNr taskNr})
		_
			# tst = case (mbTaskVal,mbTaskNr) of
				// If the task is new, but has run in a different context, initialize the states of the task and its subtasks
				(Nothing,Just initTaskNr)	= copyTaskStates initTaskNr taskNr tst
				_							= tst
			// Execute task function
			# tst = taskFuncEdit {tst & newTask = isNothing mbTaskVal && isNothing mbTaskNr}
			= (TaskBusy,tst)

applyTaskCommit :: !(Task a) !(Maybe (!Int,!TaskContainerType)) !*TSt -> (!TaskResult a,!*TSt) | iTask a
applyTaskCommit task=:{properties=properties=:{TaskProperties|isControlTask}, mbTaskNr, taskFuncCommit} mbParChildInfo tst=:{taskNr,tree,interactionLayout=tstInteractionLayout,parallelLayout=tstParallelLayout,resultLayout=tstResultLayout}
	# taskId								= iTaskId taskNr ""
	# (taskVal,tst)							= accIWorldTSt (loadValue taskId) tst
	// overwrite layouts if task defines new one, is inherited by children
	# interactionLayout						= fromMaybe tstInteractionLayout task.mbInteractionLayout
	# parallelLayout						= fromMaybe tstParallelLayout task.mbParallelLayout
	# resultLayout							= fromMaybe tstResultLayout task.mbResultLayout
	# taskInfo =	{ TaskInfo
					| taskId				= taskNrToString taskNr
					, title					= properties.TaskProperties.taskDescription.TaskDescription.title
					, description			= toString properties.TaskProperties.taskDescription.TaskDescription.description
					, type					= properties.TaskProperties.interactionType
					, isControlTask			= properties.TaskProperties.isControlTask
					, localInteraction		= properties.TaskProperties.localInteraction
					, interactionLayout		= TIInteractionLayoutMerger interactionLayout
					, parallelLayout		= TIParallelLayoutMerger parallelLayout
					, resultLayout			= TIResultLayoutMerger resultLayout
					}
	# tst = {TSt|tst & taskInfo = taskInfo, newTask = isNothing taskVal, interactionLayout = interactionLayout, parallelLayout = parallelLayout, resultLayout = resultLayout}
	# (res,tst) = case taskVal of
		Just (TaskFinished a)
			# tst = addTaskNode (TTFinishedTask taskInfo (visualizeAsHtmlDisplay a,toJSON a) False) tst
			= (TaskFinished a, {tst & taskNr = incTaskNr taskNr})
		_
			// Execute task function
			# (result, tst=:{tree=node})	= taskFuncCommit tst
			// Update task state
			= case result of
				TaskFinished a
					//If a process is finished (tl taskNr == procId), remove the process from the process DB (if it's allowed to be deleted)
					# procId				= taskNrToString (tl taskNr)
					# (gc,tst)				= if(procId == tst.TSt.properties.systemProperties.SystemProperties.taskId)
													(garbageCollectTaskInstance procId {TSt | tst & tree=node})
													(False,tst)
					//Garbage collect task store
					# tst					= deleteTaskStates taskNr tst					
					// Store final value if the process is not garbage collected
					# tst					= if (gc) tst (appIWorldTSt (storeValue taskId result) tst)
					# tst					= addTaskNode (TTFinishedTask taskInfo (visualizeAsHtmlDisplay a,toJSON a) False)
												{tst & taskNr = incTaskNr taskNr, tree = tree}
					= (TaskFinished a, tst)
				TaskBusy
					// Store intermediate value	
					# tst					= addTaskNode node
												{tst & taskNr = incTaskNr taskNr, tree = tree}
					# tst					= appIWorldTSt (storeValue taskId result) tst
					= (TaskBusy, tst)
				TaskException e str
					// Store exception
					# tst					= appIWorldTSt (storeValue taskId result) tst
					# tst					= addTaskNode (TTFinishedTask taskInfo (renderException str) True)
												{tst & taskNr = incTaskNr taskNr, tree = tree}
					= (TaskException e str, tst)
	= (res,{TSt | tst & interactionLayout = tstInteractionLayout, parallelLayout = tstParallelLayout, resultLayout = tstResultLayout})
where	
	//Add a new node to the current sequence or process
	addTaskNode node tst=:{tree} = case tree of
		TTParallelTask ti tasks = case mbParChildInfo of
			Just (idx,containerType)	= {tst & tree = TTParallelTask ti [TTParallelContainer idx containerType node:tasks]}	//Add the node to the parallel set
			Nothing						= abort "can't add task node to Parallel without index and container type specified"
		TTFinishedTask _ _ _			= {tst & tree = node} 																	//Just replace the node
		_								= {tst & tree = tree}

setInteractionFuncs	:: !TTNNInteractionTask !*TSt -> *TSt
setInteractionFuncs funcs tst=:{tree}
	= case tree of
		TTInteractionTask info _		= {tst & tree = TTInteractionTask info funcs}
		_								= tst

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
	= if (storeResult) (storeValue key result iworld) iworld
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
		= appIWorldTSt (storeValue (taskId+++"-container") (dynamic (Container value) :: Container a^ a^)) tst

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

getEditEvent :: !*TSt -> (!Maybe (!DataPath,!JSONNode),!*TSt)
getEditEvent tst=:{editEvent,taskNr}
	# mbE = case editEvent of
		Just (eventTaskId,dp,value) | eventTaskId == taskNrToString taskNr
			= Just (s2dp dp,value)
		_
			= Nothing
	= (mbE,tst)

getValueEvent :: !*TSt -> (!Maybe a,!*TSt) | JSONDecode{|*|} a
getValueEvent tst = (Nothing,tst)

getActionEvent :: !*TSt -> (!Maybe String,!*TSt)
getActionEvent tst=:{commitEvent,taskNr} = case commitEvent of
	Just (eventTaskId,name) | eventTaskId == taskNrToString taskNr
		= (Just name,{tst & commitEvent = Nothing}) // remove commit event to only process it once
	_
		= (Nothing,tst)

resetSequence :: !*TSt -> *TSt
resetSequence tst = {tst & tree = initTree initTaskInfo}
deleteTaskStates :: !TaskNr !*TSt -> *TSt
deleteTaskStates taskNr tst
	// Delete tmpFiles & values in the data store
	# tst = appIWorldTSt (deleteTmpFiles taskNr) tst
	= appIWorldTSt (deleteValues (iTaskId taskNr "")) tst
	
copyTaskStates :: !TaskNr !TaskNr !*TSt	-> *TSt
copyTaskStates fromtask totask tst
	// Copy values in the data store
	# tst 	= appIWorldTSt (copyValues (iTaskId fromtask "") (iTaskId totask "")) tst
	// Copy subprocess in the process table
	# tst	= copySubProcesses (taskNrToString fromtask) (taskNrToString totask) tst
	= tst
	
deleteTmpFiles :: !id !*IWorld -> *IWorld | iTaskId id
deleteTmpFiles taskId iworld=:{world,tmpDirectory}
	# (res, world)			= readDirectory tmpDirectory world
	| isError res			= {iworld & world = world} //Ignore error
	# world					= seq (map checkEntry (fromOk res)) world
	= {iworld & world = world}
where
	prefix = iTaskId taskId ""

	checkEntry fileName world
		| startsWith prefix fileName
			= snd (deleteFile (tmpDirectory </> fileName) world)
		| otherwise
			= world

noOutput 				= abort "Task tree node without output."
noProcessResult 		= finishedStrOutput "Cannot load result."
finishedStrOutput str	= (Text str,JSONString str)
initTree info = TTFinishedTask info noOutput False
