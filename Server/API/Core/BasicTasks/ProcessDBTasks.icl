implementation module ProcessDBTasks

import StdOverloaded, StdClass, StdInt, StdArray, StdTuple, StdList, Error, Shared
import TSt, Time, Store
from ProcessDB	import :: Process{..}, :: Menu
from ProcessDB	import qualified class ProcessDB(..), instance ProcessDB TSt, instance ProcessDB IWorld
from UserDB		import class UserDB(..), instance UserDB TSt
from Types		import :: ProcessId, :: ProcessRef

class toProcessId a where toProcessId :: a -> ProcessId

instance toProcessId ProcessId
where
	toProcessId pid = pid
	
instance toProcessId (ProcessRef a)
where
	toProcessId (ProcessRef pid) = pid
	
derive gVisualize	Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive gUpdate		Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive gDefaultMask	Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive gVerify		Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive bimap Maybe,(,)

getProcess :: !pid -> Task (Maybe Process) | toProcessId pid
getProcess pid = mkInstantTask ("Get process", "Read a process from the database.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcess (toProcessId pid) tst))

getProcessForUser :: !User !pid -> Task (Maybe Process) | toProcessId pid
getProcessForUser user pid = mkInstantTask ("Get process for user", "Read a process from the database with a check if a user needs to work on it.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessForUser user (toProcessId pid) tst))

getProcessForManager :: !User !pid -> Task (Maybe Process) | toProcessId pid
getProcessForManager man pid = mkInstantTask ("Get process for manager", "Read a process from the database with a check if it is managed by a specific user.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessForManager man (toProcessId pid) tst))

getProcesses :: ![pid] -> Task [Process] | toProcessId pid
getProcesses ids = mkInstantTask ("Get processes", "Read a set of processes from the database.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessesById (map toProcessId ids) tst))

getProcessesWithStatus :: ![TaskStatus] -> Task [Process]
getProcessesWithStatus statuses = mkInstantTask ("Get processes by status", "Read all processes from the database with a specific status.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcesses statuses tst))

getProcessesForUser	:: !User ![TaskStatus] -> Task [Process]
getProcessesForUser user statuses = mkInstantTask ("Get processes for user", "Read all processes from the database that a user needs to work on.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessesForUser user statuses tst))

getProcessOwner :: !pid -> Task (Maybe User) | toProcessId pid
getProcessOwner pid = mkInstantTask ("Get process owner", "Determine the user working on the task.") getProcessStatus`
where
	getProcessStatus` tst 
	# (process,tst)	= 'ProcessDB'.getProcess (toProcessId pid) tst
	# owner 		= if (isNothing process) Nothing (Just (fromJust process).Process.properties.managerProperties.worker)
	= (TaskFinished owner,tst)
	
setProcessOwner :: !User !pid -> Task Void | toProcessId pid
setProcessOwner user pid = mkInstantTask ("Set process owner", "Set the user working on the task.") setProcessOwner`
where
	setProcessOwner` tst=:{staticInfo}
		# (_,tst)			= 'ProcessDB'.setProcessOwner user (toProcessId pid) tst
		= (TaskFinished Void,tst)

getProcessStatus :: !pid -> Task TaskStatus | toProcessId pid
getProcessStatus pid = mkInstantTask ("Get process status", "Determine the status of a process.") getProcessStatus`
where
	getProcessStatus` tst
		# (mbProcess,tst)	= 'ProcessDB'.getProcess (toProcessId pid) tst
		= case mbProcess of
			Just proc	= (TaskFinished proc.Process.properties.systemProperties.SystemProperties.status, tst)
			Nothing		= (TaskFinished Deleted, tst)
			

activateProcess	:: !pid	-> Task Void | toProcessId pid
activateProcess pid = mkInstantTask ("Activate process", "Set the status of a process to active.") activateProcess`
where
	activateProcess` tst
		# (_,tst)	= 'ProcessDB'.setProcessStatus Active (toProcessId pid) tst
		= (TaskFinished Void,tst)
		
suspendProcess :: !pid -> Task Void	| toProcessId pid
suspendProcess pid = mkInstantTask ("Suspend process", "Set the status of a process to suspended.") suspendProcess`
where
	suspendProcess` tst
		# (_,tst)	= 'ProcessDB'.setProcessStatus Suspended (toProcessId pid) tst
		= (TaskFinished Void,tst)
		
deleteProcess :: pid -> Task Void | toProcessId pid
deleteProcess pid = mkInstantTask ("Delete process", "Delete a process from the database.") deleteProcess`
where
	deleteProcess` tst
		# (_,tst)	= 'ProcessDB'.deleteProcess (toProcessId pid) tst
		= (TaskFinished Void,tst)

sharedProcessResult :: !(ProcessRef a) -> ReadOnlyShared (Maybe a) | iTask a
sharedProcessResult (ProcessRef pid) = Shared read write getTimestamp
where
	read iworld
		# (mbResult,iworld)					= loadProcessResult (taskNrFromString pid) iworld	
		= case mbResult of
			Just (TaskFinished (a :: a^))	= (Ok (Just a),iworld)	
			_								= (Ok Nothing,iworld) //Ignore all other cases
			
	write _ iworld = (Ok Void,iworld)
	
	getTimestamp iworld=:{IWorld|timestamp} = (Ok timestamp,iworld)
		
sharedProcess :: !pid -> ReadOnlyShared (Maybe Process) | toProcessId pid
sharedProcess pid = makeReadOnlyShared read
where
	read iworld = 'ProcessDB'.getProcess (toProcessId pid) iworld
