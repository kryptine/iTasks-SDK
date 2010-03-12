implementation module ProcessDBTasks

import StdOverloaded, StdClass, StdInt, StdArray, StdTuple, StdList
import TSt

from ProcessDB import :: Process{..}, :: ProcessStatus(..), :: Menu
from ProcessDB import qualified class ProcessDB(..)
from ProcessDB import qualified instance ProcessDB TSt

from UserDB import getUser

from Types	import :: ProcessId, :: ProcessRef, :: UserId

import Time
import CommonCombinators

import Store

derive gVisualize	ProcessRef, Process, ProcessStatus, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp, TaskParallelType
derive gUpdate		ProcessRef, Process, ProcessStatus, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp, TaskParallelType
derive gPrint		ProcessRef, Process, ProcessStatus, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp, TaskParallelType
derive gParse		ProcessRef, Process, ProcessStatus, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp, TaskParallelType

derive bimap	Maybe, (,)

class toProcessId a where toProcessId :: a -> ProcessId

instance toProcessId ProcessId
where
	toProcessId pid = pid
	
instance toProcessId (ProcessRef a)
where
	toProcessId (ProcessRef pid) = pid

getProcess :: !pid -> Task (Maybe Process) | toProcessId pid
getProcess pid = mkInstantTask "getProcess" (mkTaskFunction (\tst -> ProcessDB@getProcess (toProcessId pid) tst))

getProcessForUser :: !UserId !pid -> Task (Maybe Process) | toProcessId pid
getProcessForUser username pid = mkInstantTask "getProcessForUser" (mkTaskFunction (\tst -> ProcessDB@getProcessForUser username (toProcessId pid) tst))

getProcesses :: ![pid] -> Task [Process] | toProcessId pid
getProcesses ids = mkInstantTask "getProcessesById" (mkTaskFunction (\tst -> ProcessDB@getProcessesById (map toProcessId ids) tst))

getProcessesWithStatus :: ![ProcessStatus] -> Task [Process]
getProcessesWithStatus statuses = mkInstantTask "getProcesses" (mkTaskFunction (\tst -> ProcessDB@getProcesses statuses tst))

getProcessesForUser	:: !UserId ![ProcessStatus] -> Task [Process]
getProcessesForUser username statuses = mkInstantTask "getProcessesForUser" (mkTaskFunction (\tst -> ProcessDB@getProcessesForUser username statuses tst))

getProcessOwner :: !pid -> Task (Maybe UserId) | toProcessId pid
getProcessOwner pid = mkInstantTask "getProcess" getProcessStatus`
where
	getProcessStatus` tst 
	# (process,tst)	= ProcessDB@getProcess (toProcessId pid) tst
	# owner 		= if (isNothing process) Nothing (Just (fromJust process).Process.properties.managerProps.TaskManagerProperties.worker)
	= (TaskFinished owner,tst)
	
setProcessOwner :: !UserId !pid -> Task Bool | toProcessId pid
setProcessOwner username pid = mkInstantTask "setProcessOwner" setProcessOwner`
where
	setProcessOwner` tst=:{staticInfo}
		# (ok,tst)			= ProcessDB@setProcessOwner username (toProcessId pid) tst
		= (TaskFinished ok,tst)

getProcessStatus :: !pid -> Task ProcessStatus | toProcessId pid
getProcessStatus pid = mkInstantTask "getProcessStatus" getProcessStatus`
where
	getProcessStatus` tst
		# (mbProcess,tst)	= ProcessDB@getProcess (toProcessId pid) tst
		= case mbProcess of
			Just {Process | status}	= (TaskFinished status, tst)
			Nothing					= (TaskFinished Deleted, tst)
			

activateProcess	:: !pid	-> Task Bool | toProcessId pid
activateProcess pid = mkInstantTask "activateProcess" activateProcess`
where
	activateProcess` tst
		# (ok,tst)	= ProcessDB@setProcessStatus Active (toProcessId pid) tst
		= (TaskFinished ok,tst)
		
suspendProcess :: !pid -> Task Bool	| toProcessId pid
suspendProcess pid = mkInstantTask "suspendProcess" suspendProcess`
where
	suspendProcess` tst
		# (ok,tst)	= ProcessDB@setProcessStatus Suspended (toProcessId pid) tst
		= (TaskFinished ok,tst)
		
deleteProcess :: pid -> Task Bool | toProcessId pid
deleteProcess pid = mkInstantTask "deleteProcess" deleteProcess`
where
	deleteProcess` tst
		# (ok,tst)	= ProcessDB@deleteProcess (toProcessId pid) tst
		= (TaskFinished ok,tst)