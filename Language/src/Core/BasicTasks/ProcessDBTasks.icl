implementation module ProcessDBTasks

import StdOverloaded, StdClass, StdInt, StdArray, StdTuple, StdList
import TSt

from ProcessDB import :: Process{..}, :: Menu

from ProcessDB import qualified class ProcessDB(..)
from ProcessDB import qualified instance ProcessDB TSt

from UserDB import class UserDB(..)
from UserDB import instance UserDB TSt

from Types	import :: ProcessId, :: ProcessRef

import Time
import CommonCombinators

import Store

derive class iTask	ProcessRef, Process, TaskProperties, SystemProperties, ManagerProperties, WorkerProperties, TaskStatus, TaskPriority, TaskProgress, TaskParallelType

derive bimap	Maybe, (,)

class toProcessId a where toProcessId :: a -> ProcessId

instance toProcessId ProcessId
where
	toProcessId pid = pid
	
instance toProcessId (ProcessRef a)
where
	toProcessId (ProcessRef pid) = pid

getProcess :: !pid -> Task (Maybe Process) | toProcessId pid
getProcess pid = mkInstantTask "Get process" "Read a process from the database."
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcess (toProcessId pid) tst))

getProcessForUser :: !User !pid -> Task (Maybe Process) | toProcessId pid
getProcessForUser user pid = mkInstantTask "Get process for user" "Read a process from the database with a check if a user needs to work on it."
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessForUser user (toProcessId pid) tst))

getProcessForManager :: !User !pid -> Task (Maybe Process) | toProcessId pid
getProcessForManager man pid = mkInstantTask "Get process for manager" "Read a process from the database with a check if it is managed by a specific user."
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessForManager man (toProcessId pid) tst))

getProcesses :: ![pid] -> Task [Process] | toProcessId pid
getProcesses ids = mkInstantTask "Get processes" "Read a set of processes from the database."
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessesById (map toProcessId ids) tst))

getProcessesWithStatus :: ![TaskStatus] -> Task [Process]
getProcessesWithStatus statuses = mkInstantTask "Get processes by status" "Read all processes from the database with a specific status."
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcesses statuses tst))

getProcessesForUser	:: !User ![TaskStatus] -> Task [Process]
getProcessesForUser user statuses = mkInstantTask "Get processes for user" "Read all processes from the database that a user needs to work on."
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessesForUser user statuses tst))

getProcessOwner :: !pid -> Task (Maybe User) | toProcessId pid
getProcessOwner pid = mkInstantTask "Get process owner" "Determine the user working on the task." getProcessStatus`
where
	getProcessStatus` tst 
	# (process,tst)	= 'ProcessDB'.getProcess (toProcessId pid) tst
	# owner 		= if (isNothing process) Nothing (Just (fromJust process).Process.properties.managerProperties.ManagerProperties.worker)
	= (TaskFinished owner,tst)
	
setProcessOwner :: !User !pid -> Task Void | toProcessId pid
setProcessOwner user pid = mkInstantTask "Set process owner" "Set the user working on the task." setProcessOwner`
where
	setProcessOwner` tst=:{staticInfo}
		# (_,tst)			= 'ProcessDB'.setProcessOwner user (toProcessId pid) tst
		= (TaskFinished Void,tst)

getProcessStatus :: !pid -> Task TaskStatus | toProcessId pid
getProcessStatus pid = mkInstantTask "Get process status" "Determine the status of a process." getProcessStatus`
where
	getProcessStatus` tst
		# (mbProcess,tst)	= 'ProcessDB'.getProcess (toProcessId pid) tst
		= case mbProcess of
			Just proc	= (TaskFinished proc.Process.properties.systemProperties.SystemProperties.status, tst)
			Nothing		= (TaskFinished Deleted, tst)
			

activateProcess	:: !pid	-> Task Void | toProcessId pid
activateProcess pid = mkInstantTask "Activate process" "Set the status of a process to active." activateProcess`
where
	activateProcess` tst
		# (_,tst)	= 'ProcessDB'.setProcessStatus Active (toProcessId pid) tst
		= (TaskFinished Void,tst)
		
suspendProcess :: !pid -> Task Void	| toProcessId pid
suspendProcess pid = mkInstantTask "Suspend process" "Set the status of a process to suspended." suspendProcess`
where
	suspendProcess` tst
		# (_,tst)	= 'ProcessDB'.setProcessStatus Suspended (toProcessId pid) tst
		= (TaskFinished Void,tst)
		
deleteProcess :: pid -> Task Void | toProcessId pid
deleteProcess pid = mkInstantTask "Delete process" "Delete a process from the database." deleteProcess`
where
	deleteProcess` tst
		# (_,tst)	= 'ProcessDB'.deleteProcess (toProcessId pid) tst
		= (TaskFinished Void,tst)