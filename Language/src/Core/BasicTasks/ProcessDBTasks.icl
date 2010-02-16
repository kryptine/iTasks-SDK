implementation module ProcessDBTasks

import StdOverloaded, StdClass, StdInt, StdArray, StdTuple, StdList
import TSt

from ProcessDB import :: Process{..}, :: ProcessStatus(..)
from ProcessDB import qualified class ProcessDB(..)
from ProcessDB import qualified instance ProcessDB TSt

from UserDB import getUser

from Types	import :: ProcessId, :: ProcessRef, :: UserName

import Time
import CommonCombinators

import Store

derive gVisualize	ProcessRef, Process, ProcessStatus, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp
derive gUpdate		ProcessRef, Process, ProcessStatus, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp
derive gPrint		ProcessRef, Process, ProcessStatus, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp
derive gParse		ProcessRef, Process, ProcessStatus, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp

derive bimap	Maybe, (,)

class toProcessId a where toProcessId :: a -> ProcessId

instance toProcessId ProcessId
where
	toProcessId pid = pid
	
instance toProcessId (ProcessRef a)
where
	toProcessId (ProcessRef pid) = pid

getProcess :: !pid -> Task (Maybe Process) | toProcessId pid
getProcess pid = mkInstantTask "getProcess" (\tst -> ProcessDB@getProcess (toProcessId pid) tst)

getProcessForUser :: !UserName !pid -> Task (Maybe Process) | toProcessId pid
getProcessForUser username pid = mkInstantTask "getProcessForUser" (\tst -> ProcessDB@getProcessForUser username (toProcessId pid) tst)

getProcesses :: ![pid] -> Task [Process] | toProcessId pid
getProcesses ids = mkInstantTask "getProcessesById" (\tst -> ProcessDB@getProcessesById (map toProcessId ids) tst)

getProcessesWithStatus :: ![ProcessStatus] -> Task [Process]
getProcessesWithStatus statuses = mkInstantTask "getProcesses" (\tst -> ProcessDB@getProcesses statuses tst)

getProcessesForUser	:: !UserName ![ProcessStatus] -> Task [Process]
getProcessesForUser username statuses = mkInstantTask "getProcessesForUser" (\tst -> ProcessDB@getProcessesForUser username statuses tst)

getProcessOwner :: !pid -> Task (Maybe UserName) | toProcessId pid
getProcessOwner pid = mkInstantTask "getProcess" getProcessStatus`
where
	getProcessStatus` tst 
	# (process,tst)	= ProcessDB@getProcess (toProcessId pid) tst
	# owner 		= if (isNothing process) Nothing (Just (fst (fromJust process).properties.managerProps.worker))
	= (owner,tst)
	
setProcessOwner :: !UserName !pid -> Task Bool | toProcessId pid
setProcessOwner username pid = mkInstantTask "setProcessOwner" setProcessOwner`
where
	setProcessOwner` tst=:{staticInfo}
		# (user,tst)		= getUser username tst
		# delegator			= staticInfo.currentSession.user //Current user is the new delegator of the process
		= ProcessDB@setProcessOwner (user.User.userName,user.User.displayName) (delegator.User.userName,delegator.User.displayName) (toProcessId pid) tst


getProcessStatus :: !pid -> Task ProcessStatus | toProcessId pid
getProcessStatus pid = mkInstantTask "getProcessStatus" getProcessStatus`
where
	getProcessStatus` tst
		# (mbProcess,tst)	= ProcessDB@getProcess (toProcessId pid) tst
		= case mbProcess of
			Just {Process | status}	= (status, tst)
			Nothing					= (Deleted, tst)
			

activateProcess	:: !pid	-> Task Bool | toProcessId pid
activateProcess pid = mkInstantTask "activateProcess" activateProcess`
where
	activateProcess` tst = ProcessDB@setProcessStatus Active (toProcessId pid) tst

suspendProcess :: !pid -> Task Bool	| toProcessId pid
suspendProcess pid = mkInstantTask "suspendProcess" suspendProcess`
where
	suspendProcess` tst	= ProcessDB@setProcessStatus Suspended (toProcessId pid) tst

deleteProcess :: pid -> Task Bool | toProcessId pid
deleteProcess pid = mkInstantTask "deleteProcess" deleteProcess`
where
	deleteProcess` tst = ProcessDB@deleteProcess (toProcessId pid) tst
