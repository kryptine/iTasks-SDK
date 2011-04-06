implementation module ProcessDB

import StdEnv, Maybe
import TSt, Store, Util, Text

derive JSONEncode	Process
derive JSONDecode	Process
derive gEq			Process
derive bimap Maybe, (,)

instance ProcessDB IWorld
where
	createProcess :: !Process !*IWorld -> (!ProcessId,!*IWorld)
	createProcess entry iworld
		# (pid,iworld)	 	= getPid iworld
		# (procs,iworld)	= processStore (\procs -> procs ++ [{Process | entry & taskId = pid, properties = {entry.Process.properties & systemProperties = {SystemProperties|entry.Process.properties.systemProperties & taskId = pid}} }]) iworld
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
							   && user == p.Process.properties.ProcessProperties.managerProperties.worker
							      ] of
			[entry]	= (Just entry, iworld)
			_		= (Nothing, iworld)
				
	getProcesses :: ![TaskStatus] ![RunningTaskStatus] !*IWorld -> (![Process], !*IWorld)
	getProcesses statusses runningStatusses iworld 
		# (procs, iworld)	= processStore id iworld
		= ([p \\ p <- procs | isMember p.Process.properties.systemProperties.SystemProperties.status statusses && isMember p.Process.properties.ProcessProperties.managerProperties.ManagerProperties.status runningStatusses], iworld)
			
	getProcessesById :: ![TaskId] !*IWorld -> (![Process], !*IWorld)
	getProcessesById ids iworld
		# (procs,iworld) 	= processStore id iworld
		= ([process \\ process <- procs | isMember process.Process.taskId ids], iworld)
	
	getProcessesForUser	:: !User ![TaskStatus] ![RunningTaskStatus] !*IWorld -> (![Process], !*IWorld)
	getProcessesForUser user statusses runningStatusses iworld
		# (procs,iworld) 	= processStore id iworld
		= ([p \\ p <- procs | p.Process.mutable && isRelevant user p && isMember p.Process.properties.systemProperties.SystemProperties.status statusses && isMember p.Process.properties.ProcessProperties.managerProperties.ManagerProperties.status runningStatusses], iworld)
	where
		isRelevant user {Process | properties}	
			//Either you are working on the task
			=  ( properties.ProcessProperties.managerProperties.worker == user)
			
	setProcessOwner	:: !User !TaskId !*IWorld	-> (!Bool, !*IWorld)
	setProcessOwner worker taskId iworld
		= updateProcess taskId (\x -> {Process | x & properties = {ProcessProperties|x.Process.properties & managerProperties = {x.Process.properties.ProcessProperties.managerProperties & worker = worker}}}) iworld
		
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
	
	updateProcessProperties :: !TaskId (ProcessProperties -> ProcessProperties) !*IWorld -> (!Bool, !*IWorld)
	updateProcessProperties taskId f iworld = updateProcess taskId (\p -> {Process |p & properties = f p.Process.properties}) iworld
	
	removeFinishedProcesses :: !*IWorld -> (!Bool, !*IWorld)
	removeFinishedProcesses iworld
		# (proc,iworld) = getProcesses [Finished] [Active,Suspended] iworld
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
	
	addDependency :: !TaskId !TaskId !*IWorld -> (!Bool,!*IWorld)
	addDependency processId dependent iworld
		= updateProcess processId (\p -> {Process |p & dependents = removeDup [dependent : p.Process.dependents]}) iworld
	
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
	getProcesses :: ![TaskStatus] ![RunningTaskStatus] !*TSt -> (![Process],!*TSt)
	getProcesses statuses runningStatusses tst = accIWorldTSt (getProcesses statuses runningStatusses) tst
	getProcessesById :: ![TaskId] !*TSt -> (![Process],!*TSt)
	getProcessesById processIds tst = accIWorldTSt (getProcessesById processIds) tst
	getProcessesForUser :: !User ![TaskStatus] ![RunningTaskStatus] !*TSt -> (![Process],!*TSt)
	getProcessesForUser user statuses runningStatusses tst = accIWorldTSt (getProcessesForUser user statuses runningStatusses) tst
	setProcessOwner	:: !User !TaskId !*TSt -> (!Bool,!*TSt)
	setProcessOwner user processId tst = accIWorldTSt (setProcessOwner user processId) tst
	setProcessStatus :: !TaskStatus !TaskId !*TSt -> (!Bool,!*TSt)
	setProcessStatus status processId tst = accIWorldTSt (setProcessStatus status processId) tst
	updateProcess :: !TaskId (Process -> Process) !*TSt -> (!Bool,!*TSt)
	updateProcess processId f tst = accIWorldTSt (updateProcess processId f) tst
	updateProcessProperties :: !TaskId (ProcessProperties -> ProcessProperties) !*TSt -> (!Bool,!*TSt)
	updateProcessProperties processId f tst = accIWorldTSt (updateProcessProperties processId f) tst
	removeFinishedProcesses :: !*TSt -> (!Bool,!*TSt)
	removeFinishedProcesses tst = accIWorldTSt removeFinishedProcesses tst
	setImmutable :: !TaskId !*TSt -> *TSt
	setImmutable prefix tst = appIWorldTSt (setImmutable prefix) tst
	addDependency :: !TaskId !TaskId !*TSt -> (!Bool,!*TSt)
	addDependency processId dependent tst = accIWorldTSt (addDependency processId dependent) tst
	copySubProcesses :: !TaskId !TaskId !*TSt -> *TSt
	copySubProcesses fromprefix toprefix tst = appIWorldTSt (copySubProcesses fromprefix toprefix) tst
	deleteSubProcesses :: !TaskId !*TSt -> *TSt
	deleteSubProcesses prefix tst = appIWorldTSt (deleteSubProcesses prefix) tst