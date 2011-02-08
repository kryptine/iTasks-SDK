implementation module ProcessDB

import StdEnv, Maybe
import TSt, Store, Util, Text

derive class iTask Process, TaskPriority, TaskParallelType, TaskProperties, WorkerProperties, ManagerProperties, SystemProperties, TaskProgress, FormWidth, TaskDescription, TaskStatus
derive bimap Maybe, (,)

instance ProcessDB IWorld
where
	createProcess :: !Process !*IWorld -> (!ProcessId,!*IWorld)
	createProcess entry iworld
		#(procs,iworld)		= processStore id iworld
		# (pid,iworld)	 	= getPid iworld
		# (procs,iworld)	= processStore (\_ -> procs ++ [{Process | entry & taskId = pid, properties = {entry.Process.properties & systemProperties = {SystemProperties|entry.Process.properties.systemProperties & taskId = pid}} }]) iworld
		= (pid, iworld)
		where
			getPid iworld
				| entry.Process.taskId <> "" = (entry.Process.taskId,iworld)
				| otherwise = getNewPid iworld
						
			getNewPid iworld
				# (mbNewPid,iworld) = loadValue "NextProcessID" iworld
				= case mbNewPid of
				(Just pid)
					# iworld = storeValue "NextProcessID" (pid+1) iworld //increment the stored counter by 1
					= (toString pid,iworld)
				Nothing
					# iworld = storeValue "NextProcessID" 2 iworld //store the next value (2)
					= ("1",iworld) //return the first value (1)
			
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
		= case [p\\ p <- procs |   p.Process.taskId == taskId
							   && (user == p.Process.properties.managerProperties.worker
							      || isMember user (map snd p.Process.properties.systemProperties.subTaskWorkers)
							      )] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)
		
	getProcessForManager :: !User !TaskId !*IWorld -> (!Maybe Process, !*IWorld)
	getProcessForManager manager taskId iworld
		# (procs,iworld) = processStore id iworld
		# managers = [p.Process.properties.systemProperties.manager \\ p <- procs | relevantProc taskId p]
		= case [p \\ p <- procs | p.Process.taskId == taskId && isMember manager managers] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld) 
	where
		relevantProc targetId {Process|taskId}		= taskId == targetId
		relevantProc _ _							= False
				
	getProcesses :: ![TaskStatus] !*IWorld -> (![Process], !*IWorld)
	getProcesses statusses iworld 
		# (procs, iworld)	= processStore id iworld
		= ([p \\ p <- procs | isMember p.Process.properties.systemProperties.SystemProperties.status statusses], iworld)
			
	getProcessesById :: ![TaskId] !*IWorld -> (![Process], !*IWorld)
	getProcessesById ids iworld
		# (procs,iworld) 	= processStore id iworld
		= ([process \\ process <- procs | isMember process.Process.taskId ids], iworld)
	
	getProcessesForUser	:: !User ![TaskStatus] !*IWorld -> (![Process], !*IWorld)
	getProcessesForUser user statusses iworld
		# (procs,iworld) 	= processStore id iworld
		= ([p \\ p <- procs | p.Process.mutable && isRelevant user p && isMember p.Process.properties.systemProperties.SystemProperties.status statusses ], iworld)
	where
		isRelevant user {Process | properties}	
			//Either you are working on the task
			=  ( properties.managerProperties.worker == user)
			//Or you are working on a subtask of this task in an open collaboration
			|| (isMember user (map snd properties.systemProperties.SystemProperties.subTaskWorkers))
			
	setProcessOwner	:: !User !TaskId !*IWorld	-> (!Bool, !*IWorld)
	setProcessOwner worker taskId iworld
		= updateProcess taskId (\x -> {Process | x & properties = {x.Process.properties & managerProperties = {x.Process.properties.managerProperties & worker = worker}}}) iworld
		
	setProcessStatus :: !TaskStatus !TaskId !*IWorld -> (!Bool,!*IWorld)
	setProcessStatus status taskId iworld
		= updateProcess taskId (\x -> {Process | x & properties = {x.Process.properties & systemProperties = {SystemProperties|x.Process.properties.systemProperties & status = status}}}) iworld

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
				   , properties		= {proc.Process.properties & systemProperties = systemProperties}
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
processStore fn iworld
	# (mbList,iworld)	= loadValue "ProcessDB" iworld
	# list 				= fn (case mbList of Nothing = []; Just list = list)
	# iworld			= storeValue "ProcessDB" list iworld 
	= (list,iworld)

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
	getProcessForManager :: !User !TaskId !*TSt -> (!Maybe Process,!*TSt)
	getProcessForManager manager processId tst = accIWorldTSt (getProcessForManager manager processId) tst
	getProcesses :: ![TaskStatus] !*TSt -> (![Process],!*TSt)
	getProcesses statuses tst = accIWorldTSt (getProcesses statuses) tst
	getProcessesById :: ![TaskId] !*TSt -> (![Process],!*TSt)
	getProcessesById processIds tst = accIWorldTSt (getProcessesById processIds) tst
	getProcessesForUser :: !User ![TaskStatus] !*TSt -> (![Process],!*TSt)
	getProcessesForUser user statuses tst = accIWorldTSt (getProcessesForUser user statuses) tst
	setProcessOwner	:: !User !TaskId !*TSt -> (!Bool,!*TSt)
	setProcessOwner user processId tst = accIWorldTSt (setProcessOwner user processId) tst
	setProcessStatus :: !TaskStatus !TaskId !*TSt -> (!Bool,!*TSt)
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