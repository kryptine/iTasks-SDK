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


createProcess :: !Process !*TSt -> (!ProcessId,!*TSt)
createProcess entry tst
	#(procs,tst)	= processStore id tst
	# pid = if (entry.Process.taskId <> "") entry.Process.taskId (getNewPid procs entry)
	# (procs,tst)	= processStore (\_ -> procs ++ [{Process | entry & taskId = pid, properties = {TaskProperties| entry.Process.properties & systemProps = {SystemProperties|entry.Process.properties.systemProps & processId = pid}} }]) tst
	= (pid, tst)
		
deleteProcess :: !TaskId !*TSt	-> (!Bool, !*TSt)
deleteProcess taskId tst 
	# (procs,tst) 	= processStore id tst
	# (nprocs,tst)	= processStore (\_ -> [process \\ process <- procs | process.Process.taskId <> taskId]) tst
	= (length procs <> length nprocs, tst)
		
getProcess :: !TaskId !*TSt -> (!Maybe Process,!*TSt)
getProcess taskId tst
	# (procs,tst) 	= processStore id tst
	= case [process \\ process <- procs | process.Process.taskId == taskId] of
		[entry]	= (Just entry, tst)
		_		= (Nothing, tst)

getProcessForUser :: !User !TaskId !*TSt -> (!Maybe Process,!*TSt)
getProcessForUser user taskId tst
	# (procs,tst) 	= processStore id tst
	#  users		= [p.Process.properties.managerProps.ManagerProperties.worker \\ p <- procs | relevantProc taskId p]
	= case [p \\ p <- procs | p.Process.taskId == taskId && isMember user users] of
		[entry]	= (Just entry, tst)
		_		= (Nothing, tst)
where
	relevantProc targetId {Process|taskId}		= taskId == targetId
	relevantProc _ _							= False
	
getProcesses :: ![ProcessStatus] !*TSt -> (![Process], !*TSt)
getProcesses statusses tst 
	# (procs, tst)	= processStore id tst
	= ([p \\ p <- procs | isMember p.Process.status statusses], tst)
		
getProcessesById :: ![TaskId] !*TSt -> (![Process], !*TSt)
getProcessesById ids tst
	# (procs,tst) 	= processStore id tst
	= ([process \\ process <- procs | isMember process.Process.taskId ids], tst)

getProcessesForUser	:: !User ![ProcessStatus] !*TSt -> (![Process], !*TSt)
getProcessesForUser user statusses tst
	# (procs,tst) 	= processStore id tst
	= ([p \\ p <- procs | p.Process.mutable && isRelevant user p && isMember p.Process.status statusses ], tst)
where
	isRelevant user {Process | properties}	
		//Either you are working on the task
		=  ( properties.managerProps.ManagerProperties.worker == user)
		//Or you are working on a subtask of this task in an open collaboration
		|| (isMember user (map snd properties.systemProps.SystemProperties.subTaskWorkers))
		
setProcessOwner	:: !User !TaskId !*TSt	-> (!Bool, !*TSt)
setProcessOwner worker taskId tst
	= updateProcess taskId (\x -> {Process | x & properties = {TaskProperties|x.Process.properties & managerProps = {ManagerProperties | x.Process.properties.managerProps & worker = worker}}}) tst
	
setProcessStatus :: !ProcessStatus !TaskId !*TSt -> (!Bool,!*TSt)
setProcessStatus status taskId tst = updateProcess taskId (\x -> {Process| x & status = status}) tst

updateProcess :: !TaskId (Process -> Process) !*TSt -> (!Bool, !*TSt)
updateProcess taskId f tst
	# (procs,tst) 	= processStore id tst
	# (nprocs,upd)	= unzip (map (update f) procs)
	# (nprocs,tst)	= processStore (\_ -> nprocs) tst
	= (or upd, tst)
where
	update f x		
		| x.Process.taskId == taskId	= (f x, True)
		| otherwise						= (x, False)

updateProcessProperties :: !TaskId (TaskProperties -> TaskProperties) !*TSt -> (!Bool, !*TSt)
updateProcessProperties taskId f tst = updateProcess taskId (\p -> {Process |p & properties = f p.Process.properties}) tst

removeFinishedProcesses :: !*TSt -> (!Bool, !*TSt)
removeFinishedProcesses tst
# (proc,tst) = getProcesses [Finished] tst
= removeFinishedProcesses` proc tst
where
	removeFinishedProcesses` :: ![Process] !*TSt -> (!Bool, !*TSt)
	removeFinishedProcesses` [] tst = (True, tst)
	removeFinishedProcesses` [p:ps] tst
	# (ok,tst)  = deleteProcess p.Process.taskId tst
	# tst		= deleteTaskStates (taskNrFromString p.Process.taskId) tst
	| ok 		= removeFinishedProcesses` ps tst
	| otherwise = (False,tst)

setImmutable :: !TaskId !*TSt -> *TSt
setImmutable prefix tst
	# (nprocs,tst)	= processStore (\procs -> [if (startsWith prefix proc.Process.taskId) {Process|proc & mutable = False} proc \\ proc <- procs]) tst
	= tst
	
copySubProcesses :: !TaskId !TaskId !*TSt -> *TSt
copySubProcesses fromprefix toprefix tst
	# (nprocs,tst)	= processStore (\procs -> flatten [copy fromprefix toprefix proc \\ proc <- procs]) tst
	= tst
where
	copy fromprefix toprefix proc
		| startsWith fromprefix proc.Process.taskId
			= [proc
			  ,{Process| proc
			   //Prefixes of process id's are updated
			   & taskId = toprefix +++ (proc.Process.taskId % (size fromprefix, size proc.Process.taskId))
			   //Prefixes of parent fields are also updated
			   , parent = case proc.Process.parent of
			   		Just par 	= Just (if (startsWith fromprefix par) (toprefix +++ (par % (size fromprefix, size par))) par)
			   		Nothing		= Nothing
			   //The new copy is mutable again
			   , mutable = True
			   }
			  ]
		| otherwise	= [proc]

deleteSubProcesses :: !TaskId !*TSt -> *TSt
deleteSubProcesses prefix tst 
	# (nprocs,tst)	= processStore (\procs -> [process \\ process <- procs | not (startsWith prefix process.Process.taskId)]) tst
	= tst

processStore ::  !([Process] -> [Process]) !*TSt -> (![Process],!*TSt) 
processStore fn tst=:{TSt|dataStore,world}
	# (mbList,dstore,world)	= loadValue "ProcessDB" dataStore world
	# list 					= fn (case mbList of Nothing = []; Just list = list)
	# dstore				= storeValue "ProcessDB" list dstore 
	= (list, {TSt|tst & dataStore = dstore, world = world})

maxPid :: [Process] -> Int
maxPid db = foldr max 0 (map (last o taskNrFromString) [taskId \\ {Process|taskId} <- db])

getNewPid :: ![Process] !Process -> TaskId
getNewPid db entry = (toString(inc(maxPid db)))

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