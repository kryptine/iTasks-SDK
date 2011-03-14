implementation module ProcessDBTasks

import StdOverloaded, StdClass, StdInt, StdArray, StdTuple, StdList, Error, Shared
import TSt, Time, Store
from ProcessDB	import :: Process{..}, :: Menu
from ProcessDB	import qualified class ProcessDB(..), instance ProcessDB TSt, instance ProcessDB IWorld
from UserDB		import class UserDB(..), instance UserDB TSt
from Types		import :: ProcessId
	
derive gVisualize	Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive gUpdate		Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive gDefaultMask	Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive gVerify		Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive bimap Maybe,(,)

getProcess :: !ProcessId -> Task (Maybe Process)
getProcess pid = mkInstantTask ("Get process", "Read a process from the database.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcess pid tst))

getProcessForUser :: !User !ProcessId -> Task (Maybe Process)
getProcessForUser user pid = mkInstantTask ("Get process for user", "Read a process from the database with a check if a user needs to work on it.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessForUser user pid tst))

getProcessForManager :: !User !ProcessId -> Task (Maybe Process)
getProcessForManager man pid = mkInstantTask ("Get process for manager", "Read a process from the database with a check if it is managed by a specific user.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessForManager man pid tst))

getProcesses :: ![ProcessId] -> Task [Process]
getProcesses ids = mkInstantTask ("Get processes", "Read a set of processes from the database.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessesById ids tst))

getProcessesWithStatus :: ![TaskStatus] -> Task [Process]
getProcessesWithStatus statuses = mkInstantTask ("Get processes by status", "Read all processes from the database with a specific status.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcesses statuses tst))

getProcessesForUser	:: !User ![TaskStatus] -> Task [Process]
getProcessesForUser user statuses = mkInstantTask ("Get processes for user", "Read all processes from the database that a user needs to work on.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessesForUser user statuses tst))

getProcessOwner :: !ProcessId -> Task (Maybe User)
getProcessOwner pid = mkInstantTask ("Get process owner", "Determine the user working on the task.") getProcessStatus`
where
	getProcessStatus` tst 
	# (process,tst)	= 'ProcessDB'.getProcess pid tst
	# owner 		= if (isNothing process) Nothing (Just (fromJust process).Process.properties.managerProperties.worker)
	= (TaskFinished owner,tst)
	
setProcessOwner :: !User !ProcessId -> Task Void
setProcessOwner user pid = mkInstantTask ("Set process owner", "Set the user working on the task.") setProcessOwner`
where
	setProcessOwner` tst=:{staticInfo}
		# (_,tst)			= 'ProcessDB'.setProcessOwner user pid tst
		= (TaskFinished Void,tst)

getProcessStatus :: !ProcessId -> Task TaskStatus
getProcessStatus pid = mkInstantTask ("Get process status", "Determine the status of a process.") getProcessStatus`
where
	getProcessStatus` tst
		# (mbProcess,tst)	= 'ProcessDB'.getProcess pid tst
		= case mbProcess of
			Just proc	= (TaskFinished proc.Process.properties.systemProperties.SystemProperties.status, tst)
			Nothing		= (TaskFinished Deleted, tst)
			

activateProcess	:: !ProcessId	-> Task Void
activateProcess pid = mkInstantTask ("Activate process", "Set the status of a process to active.") activateProcess`
where
	activateProcess` tst
		# (_,tst)	= 'ProcessDB'.setProcessStatus Active pid tst
		= (TaskFinished Void,tst)
		
suspendProcess :: !ProcessId -> Task Void
suspendProcess pid = mkInstantTask ("Suspend process", "Set the status of a process to suspended.") suspendProcess`
where
	suspendProcess` tst
		# (_,tst)	= 'ProcessDB'.setProcessStatus Suspended pid tst
		= (TaskFinished Void,tst)
		
deleteProcess :: !ProcessId -> Task Void
deleteProcess pid = mkInstantTask ("Delete process", "Delete a process from the database.") deleteProcess`
where
	deleteProcess` tst
		# (_,tst)	= 'ProcessDB'.deleteProcess pid tst
		= (TaskFinished Void,tst)
