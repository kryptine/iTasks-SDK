implementation module ProcessDBTasks

import StdOverloaded, StdClass, StdInt, StdArray, StdTuple, StdList
import TSt

from ProcessDB import :: Process{..}, :: ProcessStatus(..), :: Menu

from ProcessDB import qualified getProcess, getProcessForUser, getProcesses, getProcessesById, getProcessesForUser
from ProcessDB import qualified setProcessOwner, setProcessStatus, deleteProcess

from UserDB import getUser

from Types	import :: ProcessId, :: ProcessRef

import Time
import CommonCombinators

import Store

derive gVisualize	ProcessRef, Process, ProcessStatus, TaskProperties, SystemProperties, ManagerProperties, WorkerProperties, TaskPriority, TaskProgress, Timestamp, TaskParallelType
derive gUpdate		ProcessRef, Process, ProcessStatus, TaskProperties, SystemProperties, ManagerProperties, WorkerProperties, TaskPriority, TaskProgress, Timestamp, TaskParallelType
derive gPrint		ProcessRef, Process, ProcessStatus, TaskProperties, SystemProperties, ManagerProperties, WorkerProperties, TaskPriority, TaskProgress, Timestamp, TaskParallelType
derive gParse		ProcessRef, Process, ProcessStatus, TaskProperties, SystemProperties, ManagerProperties, WorkerProperties, TaskPriority, TaskProgress, Timestamp, TaskParallelType
derive gError		ProcessRef, Process, ProcessStatus, TaskProperties, SystemProperties, ManagerProperties, WorkerProperties, TaskPriority, TaskProgress, Timestamp, TaskParallelType
derive gHint		ProcessRef, Process, ProcessStatus, TaskProperties, SystemProperties, ManagerProperties, WorkerProperties, TaskPriority, TaskProgress, Timestamp, TaskParallelType

derive bimap	Maybe, (,)

class toProcessId a where toProcessId :: a -> ProcessId

instance toProcessId ProcessId
where
	toProcessId pid = pid
	
instance toProcessId (ProcessRef a)
where
	toProcessId (ProcessRef pid) = pid

getProcess :: !pid -> Task (Maybe Process) | toProcessId pid
getProcess pid = mkInstantTask "getProcess" (mkTaskFunction (\tst -> 'ProcessDB'.getProcess (toProcessId pid) tst))

getProcessForUser :: !User !pid -> Task (Maybe Process) | toProcessId pid
getProcessForUser user pid = mkInstantTask "getProcessForUser" (mkTaskFunction (\tst -> 'ProcessDB'.getProcessForUser user (toProcessId pid) tst))

getProcesses :: ![pid] -> Task [Process] | toProcessId pid
getProcesses ids = mkInstantTask "getProcessesById" (mkTaskFunction (\tst -> 'ProcessDB'.getProcessesById (map toProcessId ids) tst))

getProcessesWithStatus :: ![ProcessStatus] -> Task [Process]
getProcessesWithStatus statuses = mkInstantTask "getProcesses" (mkTaskFunction (\tst -> 'ProcessDB'.getProcesses statuses tst))

getProcessesForUser	:: !User ![ProcessStatus] -> Task [Process]
getProcessesForUser user statuses = mkInstantTask "getProcessesForUser" (mkTaskFunction (\tst -> 'ProcessDB'.getProcessesForUser user statuses tst))

getProcessOwner :: !pid -> Task (Maybe User) | toProcessId pid
getProcessOwner pid = mkInstantTask "getProcess" getProcessStatus`
where
	getProcessStatus` tst 
	# (process,tst)	= 'ProcessDB'.getProcess (toProcessId pid) tst
	# owner 		= if (isNothing process) Nothing (Just (fromJust process).Process.properties.managerProps.ManagerProperties.worker)
	= (TaskFinished owner,tst)
	
setProcessOwner :: !User !pid -> Task Void | toProcessId pid
setProcessOwner user pid = mkInstantTask "setProcessOwner" setProcessOwner`
where
	setProcessOwner` tst=:{staticInfo}
		# (_,tst)			= 'ProcessDB'.setProcessOwner user (toProcessId pid) tst
		= (TaskFinished Void,tst)

getProcessStatus :: !pid -> Task ProcessStatus | toProcessId pid
getProcessStatus pid = mkInstantTask "getProcessStatus" getProcessStatus`
where
	getProcessStatus` tst
		# (mbProcess,tst)	= 'ProcessDB'.getProcess (toProcessId pid) tst
		= case mbProcess of
			Just {Process | status}	= (TaskFinished status, tst)
			Nothing					= (TaskFinished Deleted, tst)
			

activateProcess	:: !pid	-> Task Void | toProcessId pid
activateProcess pid = mkInstantTask "activateProcess" activateProcess`
where
	activateProcess` tst
		# (_,tst)	= 'ProcessDB'.setProcessStatus Active (toProcessId pid) tst
		= (TaskFinished Void,tst)
		
suspendProcess :: !pid -> Task Void	| toProcessId pid
suspendProcess pid = mkInstantTask "suspendProcess" suspendProcess`
where
	suspendProcess` tst
		# (_,tst)	= 'ProcessDB'.setProcessStatus Suspended (toProcessId pid) tst
		= (TaskFinished Void,tst)
		
deleteProcess :: pid -> Task Void | toProcessId pid
deleteProcess pid = mkInstantTask "deleteProcess" deleteProcess`
where
	deleteProcess` tst
		# (_,tst)	= 'ProcessDB'.deleteProcess (toProcessId pid) tst
		= (TaskFinished Void,tst)