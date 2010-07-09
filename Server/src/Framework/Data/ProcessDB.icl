implementation module ProcessDB

import StdEnv, StdGeneric, StdMaybe, GenEq
import TSt, Store, Util, Text

getActionIcon :: !Action -> String
getActionIcon (ActionIcon _ icon)	= icon
getActionIcon ActionOk				= "icon-ok"
getActionIcon ActionCancel			= "icon-cancel"
getActionIcon ActionYes				= "icon-yes"
getActionIcon ActionNo				= "icon-no"
getActionIcon ActionNext			= "icon-next"
getActionIcon ActionPrevious		= "icon-previous"
getActionIcon ActionFinish			= "icon-finish"
getActionIcon ActionNew				= "icon-new"
getActionIcon ActionOpen			= "icon-open"
getActionIcon ActionSaveAs			= "icon-save-as"
getActionIcon ActionSave			= "icon-save"
getActionIcon ActionQuit			= "icon-exit"
getActionIcon ActionClose			= "icon-cancel"
getActionIcon ActionHelp			= "icon-help"
getActionIcon ActionShowAbout		= "icon-help"
getActionIcon ActionFind			= "icon-find"
getActionIcon _						= ""

instance == ProcessStatus
where
	(==) Active		Active		= True
	(==) Suspended	Suspended	= True
	(==) Finished	Finished	= True
	(==) Deleted	Deleted		= True
	(==) _			_			= False

instance toString ProcessStatus
where
	toString Active		= "Active"
	toString Suspended	= "Suspended"
	toString Finished	= "Finished"
	toString Deleted	= "Deleted"

instance ProcessDB IWorld
where
	createProcess :: !Process !*IWorld -> (!ProcessId,!*IWorld)
	createProcess entry iworld
		#(procs,iworld=:{store,world})	= processStore id iworld
		# (pid,store,world) 			= getPid store world
		# (procs,iworld)				= processStore (\_ -> procs ++ [{Process | entry & taskId = pid, properties = {TaskProperties| entry.Process.properties & systemProperties = {SystemProperties|entry.Process.properties.systemProperties & taskId = pid}} }]) {iworld & store = store, world = world}
		= (pid, iworld)
		where
			getPid store world
				| entry.Process.taskId <> "" = (entry.Process.taskId,store,world)
				| otherwise = getNewPid store world
						
			getNewPid store world
				# (mbNewPid,store,world) = loadValue "NextProcessID" store world
				= case mbNewPid of
				(Just pid)
					# store = storeValue "NextProcessID" (pid+1) store //increment the stored counter by 1
					= (toString pid,store,world)
				Nothing
					# store = storeValue "NextProcessID" 2 store //store the next value (2)
					= ("1",store,world) //return the first value (1)
			
	deleteProcess :: !TaskId !*IWorld	-> (!Bool, !*IWorld)
	deleteProcess taskId iworld 
		# (procs,iworld) 	= processStore id iworld
		# (nprocs,iworld)	= processStore (\_ -> [process \\ process <- procs | process.Process.taskId <> taskId]) iworld
		= (length procs <> length nprocs, iworld)
			
	getProcess :: !TaskId !*IWorld -> (!Maybe Process,!*IWorld)
	getProcess taskId iworld
		# (procs,iworld) 	= processStore id iworld
		= case [process \\ process <- procs | process.Process.taskId == taskId] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)
	
	getProcessForUser :: !User !TaskId !*IWorld -> (!Maybe Process,!*IWorld)
	getProcessForUser user taskId iworld
		# (procs,iworld) 	= processStore id iworld
		#  users		= [p.Process.properties.managerProperties.ManagerProperties.worker \\ p <- procs | relevantProc taskId p]
		= case [p \\ p <- procs | p.Process.taskId == taskId && isMember user users] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)
	where
		relevantProc targetId {Process|taskId}		= taskId == targetId
		relevantProc _ _							= False
		
	getProcesses :: ![ProcessStatus] !*IWorld -> (![Process], !*IWorld)
	getProcesses statusses iworld 
		# (procs, iworld)	= processStore id iworld
		= ([p \\ p <- procs | isMember p.Process.status statusses], iworld)
			
	getProcessesById :: ![TaskId] !*IWorld -> (![Process], !*IWorld)
	getProcessesById ids iworld
		# (procs,iworld) 	= processStore id iworld
		= ([process \\ process <- procs | isMember process.Process.taskId ids], iworld)
	
	getProcessesForUser	:: !User ![ProcessStatus] !*IWorld -> (![Process], !*IWorld)
	getProcessesForUser user statusses iworld
		# (procs,iworld) 	= processStore id iworld
		= ([p \\ p <- procs | p.Process.mutable && isRelevant user p && isMember p.Process.status statusses ], iworld)
	where
		isRelevant user {Process | properties}	
			//Either you are working on the task
			=  ( properties.managerProperties.ManagerProperties.worker == user)
			//Or you are working on a subtask of this task in an open collaboration
			|| (isMember user (map snd properties.systemProperties.SystemProperties.subTaskWorkers))
			
	setProcessOwner	:: !User !TaskId !*IWorld	-> (!Bool, !*IWorld)
	setProcessOwner worker taskId iworld
		= updateProcess taskId (\x -> {Process | x & properties = {TaskProperties|x.Process.properties & managerProperties = {ManagerProperties | x.Process.properties.managerProperties & worker = worker}}}) iworld
		
	setProcessStatus :: !ProcessStatus !TaskId !*IWorld -> (!Bool,!*IWorld)
	setProcessStatus status taskId iworld = updateProcess taskId (\x -> {Process| x & status = status}) iworld
	
	updateProcess :: !TaskId (Process -> Process) !*IWorld -> (!Bool, !*IWorld)
	updateProcess taskId f iworld
		# (procs,iworld) 	= processStore id iworld
		# (nprocs,upd)	= unzip (map (update f) procs)
		# (nprocs,iworld)	= processStore (\_ -> nprocs) iworld
		= (or upd, iworld)
	where
		update f x		
			| x.Process.taskId == taskId	= (f x, True)
			| otherwise						= (x, False)
	
	updateProcessProperties :: !TaskId (TaskProperties -> TaskProperties) !*IWorld -> (!Bool, !*IWorld)
	updateProcessProperties taskId f iworld = updateProcess taskId (\p -> {Process |p & properties = f p.Process.properties}) iworld
	
	removeFinishedProcesses :: !*IWorld -> (!Bool, !*IWorld)
	removeFinishedProcesses iworld
		# (proc,iworld) = getProcesses [Finished] iworld
		= removeFinishedProcesses` proc iworld
	where
		removeFinishedProcesses` :: ![Process] !*IWorld -> (!Bool, !*IWorld)
		removeFinishedProcesses` [] iworld = (True, iworld)
		removeFinishedProcesses` [p:ps] iworld
		# (ok,iworld)	= deleteProcess p.Process.taskId iworld
		| ok 		= removeFinishedProcesses` ps iworld
		| otherwise = (False,iworld)
	
	setImmutable :: !TaskId !*IWorld -> *IWorld
	setImmutable prefix iworld
		# (nprocs,iworld)	= processStore (\procs -> [if (startsWith prefix proc.Process.taskId) {Process|proc & mutable = False} proc \\ proc <- procs]) iworld
		= iworld
		
	copySubProcesses :: !TaskId !TaskId !*IWorld -> *IWorld
	copySubProcesses fromprefix toprefix iworld
		# (nprocs,iworld)	= processStore (\procs -> flatten [copy fromprefix toprefix proc \\ proc <- procs]) iworld
		= iworld
	where
		copy fromprefix toprefix proc
			| startsWith fromprefix proc.Process.taskId
				//Prefix of task id has got to be updated
				# taskId = toprefix +++ (proc.Process.taskId % (size fromprefix, size proc.Process.taskId))
				//Update properties
				# systemProperties =
					{SystemProperties|proc.Process.properties.systemProperties
					//Prefix of task id has got to be updated
					& taskId = taskId
					//Prefixes of parent fields are also updated
					, parent = case proc.Process.properties.systemProperties.parent of
						Just par	= Just (if (startsWith fromprefix par) (toprefix +++ (par % (size fromprefix, size par))) par)
						Nothing		= Nothing
				 	}
				= [proc
				  ,{Process| proc
				   & taskId			= taskId
				   , properties		= {TaskProperties|proc.Process.properties & systemProperties = systemProperties}
				   //The new copy is mutable again
				   , mutable 		= True
				   }
				  ]
			| otherwise	= [proc]
	
	deleteSubProcesses :: !TaskId !*IWorld -> *IWorld
	deleteSubProcesses prefix iworld 
		# (nprocs,iworld)	= processStore (\procs -> [process \\ process <- procs | not (startsWith prefix process.Process.taskId)]) iworld
		= iworld

processStore ::  !([Process] -> [Process]) !*IWorld -> (![Process],!*IWorld) 
processStore fn iworld=:{IWorld|store,world}
	# (mbList,store,world)	= loadValue "ProcessDB" store world
	# list 					= fn (case mbList of Nothing = []; Just list = list)
	# store					= storeValue "ProcessDB" list store 
	= (list, {IWorld| iworld & store = store, world = world})

/*
Deprecated! The next PID is now read from the store

maxPid :: [Process] -> Int
maxPid db = foldr max 0 (map (last o taskNrFromString) [taskId \\ {Process|taskId} <- db])

getNewPid :: ![Process] !Process -> TaskId
getNewPid db entry = (toString(inc(maxPid db)))
*/



instance ProcessDB TSt
where
	createProcess :: !Process !*TSt -> (!ProcessId,!*TSt)
	createProcess process tst = accIWorldTSt (createProcess process) tst
	deleteProcess :: !TaskId !*TSt -> (!Bool,!*TSt)
	deleteProcess processId tst = accIWorldTSt (deleteProcess processId) tst
	getProcess :: !TaskId !*TSt -> (!Maybe Process,!*TSt)
	getProcess processId tst = accIWorldTSt (getProcess processId) tst
	getProcessForUser :: !User !TaskId !*TSt -> (!Maybe Process,!*TSt)
	getProcessForUser user processId tst = accIWorldTSt (getProcessForUser user processId) tst
	getProcesses :: ![ProcessStatus] !*TSt -> (![Process],!*TSt)
	getProcesses statuses tst = accIWorldTSt (getProcesses statuses) tst
	getProcessesById :: ![TaskId] !*TSt -> (![Process],!*TSt)
	getProcessesById processIds tst = accIWorldTSt (getProcessesById processIds) tst
	getProcessesForUser :: !User ![ProcessStatus] !*TSt -> (![Process],!*TSt)
	getProcessesForUser user statuses tst = accIWorldTSt (getProcessesForUser user statuses) tst
	setProcessOwner	:: !User !TaskId !*TSt -> (!Bool,!*TSt)
	setProcessOwner user processId tst = accIWorldTSt (setProcessOwner user processId) tst
	setProcessStatus :: !ProcessStatus !TaskId !*TSt -> (!Bool,!*TSt)
	setProcessStatus status processId tst = accIWorldTSt (setProcessStatus status processId) tst
	updateProcess :: !TaskId (Process -> Process) !*TSt -> (!Bool,!*TSt)
	updateProcess processId f tst = accIWorldTSt (updateProcess processId f) tst
	updateProcessProperties :: !TaskId (TaskProperties -> TaskProperties) !*TSt -> (!Bool,!*TSt)
	updateProcessProperties processId f tst = accIWorldTSt (updateProcessProperties processId f) tst
	removeFinishedProcesses :: !*TSt -> (!Bool,!*TSt)
	removeFinishedProcesses tst = accIWorldTSt removeFinishedProcesses tst
	setImmutable :: !TaskId !*TSt -> *TSt
	setImmutable prefix tst = appIWorldTSt (setImmutable prefix) tst
	copySubProcesses :: !TaskId !TaskId !*TSt -> *TSt
	copySubProcesses fromprefix toprefix tst = appIWorldTSt (copySubProcesses fromprefix toprefix) tst
	deleteSubProcesses :: !TaskId !*TSt -> *TSt
	deleteSubProcesses prefix tst = appIWorldTSt (deleteSubProcesses prefix) tst
	
derive gVisualize	Action
derive gUpdate		Action
derive gPrint		Action
derive gParse		Action
derive gEq			Action
derive gHint		Action
derive gError		Action

derive bimap Maybe, (,)

instance == Action
where
	(==) :: !Action !Action -> Bool
	(==) (ActionParam label0 param0) (ActionParam label1 param1)
		= label0 == label1 && (param0 == param1 || param0 == "?")
	(==) a b = gEq{|*|} a b