implementation module ProcessDB

import StdEnv, StdGeneric, StdMaybe, GenEq
import TSt, Store, Util

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
getActionIcon ActionHelp			= "icon-help"
getActionIcon ActionShowAbout		= "icon-help"
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

instance ProcessDB TSt
where
	createProcess :: !Process !*TSt -> (!ProcessId,!*TSt)
	createProcess entry tst
		#(procs,tst)	= processStore id tst
		# pid = if (entry.Process.processId <> "") entry.Process.processId (getNewPid procs entry)
		# (procs,tst)	= processStore (\_ -> procs ++ [{Process | entry & processId = pid, properties = {TaskProperties| entry.Process.properties & systemProps = {TaskSystemProperties|entry.Process.properties.systemProps & processId = pid}} }]) tst
		= (pid, tst)
		
	deleteProcess :: !ProcessId !*TSt	-> (!Bool, !*TSt)
	deleteProcess processId tst 
		# (procs,tst) 	= processStore id tst
		# (nprocs,tst)	= processStore (\_ -> [process \\ process <- procs | process.Process.processId <> processId]) tst
		= (length procs <> length nprocs, tst)
		
	getProcess :: !ProcessId !*TSt -> (!Maybe Process,!*TSt)
	getProcess processId tst
		# (procs,tst) 	= processStore id tst
		= case [process \\ process <- procs | process.Process.processId == processId] of
			[entry]	= (Just entry, tst)
			_		= (Nothing, tst)

	getProcessForUser :: !UserId !ProcessId !*TSt -> (!Maybe Process,!*TSt)
	getProcessForUser userName processId tst
		# (procs,tst) 	= processStore id tst
		#  usernames	= [p.Process.properties.managerProps.TaskManagerProperties.worker \\ p <- procs | relevantProc processId p]
		= case [p \\ p <- procs | p.Process.processId == processId && isMember userId (map  toUserId usernames)] of
			[entry]	= (Just entry, tst)
			_		= (Nothing, tst)
	where
		userId 										= toUserId userName
		relevantProc targetId {Process|processId}	= processId == targetId
		relevantProc _ _							= False
	
	getProcesses :: ![ProcessStatus] !*TSt -> (![Process], !*TSt)
	getProcesses statusses tst 
		# (procs, tst)	= processStore id tst
		= ([p \\ p <- procs | isMember p.Process.status statusses], tst)
			
	getProcessesById :: ![ProcessId] !*TSt -> (![Process], !*TSt)
	getProcessesById ids tst
		# (procs,tst) 	= processStore id tst
		= ([process \\ process <- procs | isMember process.Process.processId ids], tst)

	getProcessesForUser	:: !UserId ![ProcessStatus] !*TSt -> (![Process], !*TSt)
	getProcessesForUser userName statusses tst
		# (procs,tst) 	= processStore id tst
		# rprocs	 	= map (relevantProc userId) procs
		# procids		= [p \\ p <- rprocs | p <> ""]
		= ([p \\ p <- procs | isMember p.Process.processId procids && isMember p.Process.status statusses], tst)
	where
		userId	= toUserId userName
		relevantProc userId {Process | processId, properties}
			| toUserId properties.managerProps.TaskManagerProperties.worker == userId	= processId
			| otherwise																	= ""
	
	getTempProcessesForUser :: !UserId ![ProcessStatus] !*TSt -> (![Process], !*TSt)
	getTempProcessesForUser userName statusses tst
		# (procs,tst) 	= processStore id tst
		# rprocs		= map (relevantProc userId) procs
		# procids		= [p \\ p <- rprocs | p <> ""]
		= ([p \\ p <- procs | isMember p.Process.processId procids && isMember p.Process.status statusses],tst)
	where
		userId = toUserId userName
		relevantProc userId {Process | processId, properties = {managerProps = {worker,tempWorkers}}}
			| isMember userId [u \\ (p,u) <- tempWorkers] && userId <> toUserId worker = processId
			| otherwise 															  	 = ""
	
	setProcessOwner	:: !UserId !ProcessId !*TSt	-> (!Bool, !*TSt)
	setProcessOwner worker processId tst
		= updateProcess processId (\x -> {Process | x & properties = {TaskProperties|x.Process.properties & managerProps = {TaskManagerProperties | x.Process.properties.managerProps & worker = worker}}}) tst
	
	setProcessStatus :: !ProcessStatus !ProcessId !*TSt -> (!Bool,!*TSt)
	setProcessStatus status processId tst = updateProcess processId (\x -> {Process| x & status = status}) tst

	//setProcessResult :: !DynamicId !ProcessId !*TSt -> (!Bool,!*TSt)
	//setProcessResult result processId tst = updateProcess processId (\x -> {Process| x & result = Just result}) tst 
	
	updateProcess :: ProcessId (Process -> Process) !*TSt -> (!Bool, !*TSt)
	updateProcess processId f tst
		# (procs,tst) 	= processStore id tst
		# (nprocs,upd)	= unzip (map (update f) procs)
		# (nprocs,tst)	= processStore (\_ -> nprocs) tst
		= (or upd, tst)
	where
		update f x		
			| x.Process.processId == processId	= (f x, True)
			| otherwise							= (x, False)

	updateProcessProperties :: !ProcessId (TaskProperties -> TaskProperties) !*TSt -> (!Bool, !*TSt)
	updateProcessProperties processId f tst = updateProcess processId (\p -> {Process |p & properties = f p.Process.properties}) tst

	removeFinishedProcesses :: !*TSt -> (!Bool, !*TSt)
	removeFinishedProcesses tst
	# (proc,tst) = getProcesses [Finished] tst
	= removeFinishedProcesses` proc tst
	where
		removeFinishedProcesses` :: ![Process] !*TSt -> (!Bool, !*TSt)
		removeFinishedProcesses` [] tst = (True, tst)
		removeFinishedProcesses` [p:ps] tst
		# (ok,tst)  = deleteProcess p.Process.processId tst
		# tst		= deleteTaskStates (taskNrFromString p.Process.processId) tst
		| ok 		= removeFinishedProcesses` ps tst
		| otherwise = (False,tst)

processStore ::  !([Process] -> [Process]) !*TSt -> (![Process],!*TSt) 
processStore fn tst=:{TSt|dataStore,world}
	# (mbList,dstore,world)	= loadValue "ProcessDB" dataStore world
	# list 					= fn (case mbList of Nothing = []; Just list = list)
	# dstore				= storeValue "ProcessDB" list dstore 
	= (list, {TSt|tst & dataStore = dstore, world = world})

maxPid :: [Process] -> Int
maxPid db = foldr max 0 (map (last o taskNrFromString) [processId \\ {Process|processId} <- db])

getNewPid :: ![Process] !Process -> ProcessId
getNewPid db entry = (toString(inc(maxPid db)))

derive gVisualize	Action
derive gUpdate		Action
derive gPrint		Action
derive gParse		Action
derive gEq			Action

derive bimap Maybe, (,)

instance == Action
where
	(==) :: !Action !Action -> Bool
	(==) (ActionParam label0 param0) (ActionParam label1 param1)
		= label0 == label1 && (param0 == param1 || param0 == "?")
	(==) a b = gEq{|*|} a b