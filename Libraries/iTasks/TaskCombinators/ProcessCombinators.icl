implementation module ProcessCombinators

import StdOverloaded, StdClass, StdInt, StdArray, StdTuple, GenBimap
import TSt

from ProcessDB import :: Process{..}, :: ProcessStatus(..), :: ProcessType(..)
from ProcessDB import qualified class ProcessDB(..)
from ProcessDB import qualified instance ProcessDB TSt
from ProcessDB import mkDynamicProcessEntry

from DynamicDB import qualified class DynamicDB(..)
from DynamicDB import qualified instance DynamicDB TSt

import UserDB

import Time
import CommonCombinators

derive gVisualize	ProcessReference, Process, ProcessStatus, ProcessType, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp
derive gUpdate		ProcessReference, Process, ProcessStatus, ProcessType, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp
derive gPrint		ProcessReference, Process, ProcessStatus, ProcessType, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp
derive gParse		ProcessReference, Process, ProcessStatus, ProcessType, TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, Timestamp

spawnProcess :: !UserId !Bool !(Task a) -> Task (ProcessReference a) | iTask a
spawnProcess uid activate task = mkInstantTask "spawnProcess" spawnProcess`
where
	spawnProcess` tst=:{TSt|mainTask}
		# (curUid,tst)		= getCurrentUser tst
		# (user,tst)		= getUser uid tst
		# (delegator,tst)	= getUser curUid tst
		# (curTime,tst) 	= accWorldTSt time tst
		# (dynId,tst)		= DynamicDB@createDynamic (dynamic createDynamicTask task) tst
		# (newPid,tst)		= ProcessDB@createProcess (entry mainTask dynId curTime (user.User.userId,user.User.displayName) (delegator.User.userId,delegator.User.displayName)) tst	
		| uid == curUid
			# tst			= addNewProcess newPid tst
			= (ProcessReference newPid, {tst & activated = True})
		| otherwise
			= (ProcessReference newPid, {tst & activated = True})
				
	entry pid did now user delegator	= mkDynamicProcessEntry (taskLabel task) did now user delegator (if activate Active Suspended) pid	// Create a process database entry
	
	//Turn a task yielding a value of type a into a value of dynamic
	createDynamicTask :: !(Task a) -> Task Dynamic | iTask a
	createDynamicTask (Task name mbCxt tf) = Task name mbCxt createDynamicTask`
	where
		createDynamicTask` tst
			# (a, tst)	= tf tst
			# dyn		= dynamic a
			= (dyn, tst)

waitForProcess :: (ProcessReference a) -> Task (Maybe a) | iTask a
waitForProcess (ProcessReference pid) = mkMonitorTask "waitForProcess" waitForProcess`
where
	waitForProcess` tst
		# (mbProcess,tst)	= ProcessDB@getProcess pid tst
		= case mbProcess of
			Just {Process | processId, status, result}
				= case status of
					Finished
						= case result of
							(Just dynid)
								# (mbResult,tst) = DynamicDB@getDynamic dynid tst
								= case mbResult of
									(Just (result :: a^))	= (Just result, {tst & activated = True}) //We are done and return the result
									_						= (Nothing, {tst & activated = True}) //We could not find the result dynamic	
							Nothing
								= (Nothing, {tst & activated = True}) //The process was finished but yielded no result	
					_
						= (Nothing, {tst & activated = False})	// We are not done yet...
			_	= (Nothing, {tst & activated = True})	//We could not find the process in our database, we are done


getProcessStatus :: (ProcessReference a) -> Task ProcessStatus | iTask a
getProcessStatus (ProcessReference pid) = mkInstantTask "getProcessStatus" getProcessStatus`
where
	getProcessStatus` tst
		# (mbProcess,tst)	= ProcessDB@getProcess pid tst
		= case mbProcess of
			Just {Process | status}	= (status, tst)
			Nothing							= (Deleted, tst)
			
getProcessOwner :: (ProcessReference a) -> Task (Maybe Int)
getProcessOwner (ProcessReference pid) = mkInstantTask "getProcess" getProcessStatus`
where
	getProcessStatus` tst 
	# (process,tst)	= ProcessDB@getProcess pid tst
	# owner = if (isNothing process) Nothing (Just (fst (fromJust process).properties.managerProps.worker))
	= (owner,tst)

activateProcess	:: (ProcessReference a)	-> Task Bool | iTask a
activateProcess (ProcessReference pid) = mkInstantTask "activateProcess" activateProcess`
where
	activateProcess` tst = ProcessDB@setProcessStatus Active pid tst

suspendProcess :: (ProcessReference a) -> Task Bool	| iTask a
suspendProcess (ProcessReference pid) = mkInstantTask "suspendProcess" suspendProcess`
where
	suspendProcess` tst	= ProcessDB@setProcessStatus Suspended pid tst

suspendCurrentProcess :: Task Bool
suspendCurrentProcess = mkInstantTask "suspendCurrentProcess" suspendCurrentProcess`
where
	suspendCurrentProcess` tst
		# (pid, tst)	= getCurrentProcess tst
		= ProcessDB@setProcessStatus Suspended pid tst
		
deleteProcess :: (ProcessReference a) -> Task Bool | iTask a
deleteProcess (ProcessReference pid) = mkInstantTask "deleteProcess" deleteProcess`
where
	deleteProcess` tst = ProcessDB@deleteProcess pid tst

deleteCurrentProcess :: Task Bool
deleteCurrentProcess = mkInstantTask "deleteCurrentProcess" deleteCurrentProcess`
where
	deleteCurrentProcess` tst
		# (pid, tst)	= getCurrentProcess tst
		= ProcessDB@deleteProcess pid tst

updateProcessOwner :: UserId (ProcessReference a) ->	Task Bool | iTask a 
updateProcessOwner uid (ProcessReference pid) = mkInstantTask "updateProcessOwner" setProcessOwner`
where
	setProcessOwner` tst
		# (curUid,tst)		= getCurrentUser tst
		# (user,tst)		= getUser uid tst
		# (delegator,tst)	= getUser curUid tst
		= ProcessDB@setProcessOwner (user.User.userId,user.User.displayName) (delegator.User.userId,delegator.User.displayName) pid tst


//New "meta" process tasks
getCurrentProcessId :: Task ProcessId
getCurrentProcessId = mkInstantTask "getCurrentProcessId" getCurrentProcessId`
where
	getCurrentProcessId` tst=:{staticInfo}
		= (staticInfo.currentProcessId,tst)

deleteProcessById :: ProcessId -> Task Bool 
deleteProcessById pid = mkInstantTask "deleteProcess" deleteProcess`
where
	deleteProcess` tst = ProcessDB@deleteProcess pid tst

getProcess :: !ProcessId -> Task (Maybe Process)
getProcess pid = mkInstantTask "getProcess" (\tst -> ProcessDB@getProcess pid tst)

getProcessForUser :: !UserId !ProcessId -> Task (Maybe Process)
getProcessForUser uid pid = mkInstantTask "getProcessForUser" (\tst -> ProcessDB@getProcessForUser uid pid tst)

getProcesses :: ![ProcessStatus] !Bool -> Task [Process]
getProcesses statuses ignoreEmbedded = mkInstantTask "getProcesses" (\tst -> ProcessDB@getProcesses statuses ignoreEmbedded tst)

getProcessesById :: ![ProcessId] -> Task [Process]
getProcessesById ids = mkInstantTask "getProcessesById" (\tst -> ProcessDB@getProcessesById ids tst)

getProcessesForUser	:: !UserId ![ProcessStatus] !Bool -> Task [Process]
getProcessesForUser uid statuses ignoreEmbedded = mkInstantTask "getProcessesForUser" (\tst -> ProcessDB@getProcessesForUser uid statuses ignoreEmbedded tst)

setProcessOwner :: !UserId !ProcessId -> Task Bool
setProcessOwner uid pid = mkInstantTask "setProcessOwner" setProcessOwner`
where
	setProcessOwner` tst
		# (cur,tst)			= getCurrentUser tst //Current user is the new delegator of the process
		# (user,tst)		= getUser uid tst
		# (delegator,tst)	= getUser cur tst
		= ProcessDB@setProcessOwner (user.User.userId,user.User.displayName) (delegator.User.userId,delegator.User.displayName) pid tst

setProcessStatus :: !ProcessStatus !ProcessId -> Task Bool
setProcessStatus status pid = mkInstantTask "setProcessStatus" (\tst -> ProcessDB@setProcessStatus status pid tst)
