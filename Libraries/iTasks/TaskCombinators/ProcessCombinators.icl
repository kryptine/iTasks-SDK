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
import iDataForms
import CommonCombinators

derive gForm	ProcessReference, Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, TaskProgress, Time
derive gUpd		ProcessReference, Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, TaskProgress, Time
derive gPrint	ProcessReference, Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, TaskProgress, Time
derive gParse	ProcessReference, Process, ProcessStatus, ProcessType, TaskProperties, TaskPriority, TaskProgress, Time

:: ProcessReference a 	= ProcessReference !Int		//We only keep the id in the process database

spawnProcess :: !UserId !Bool !(LabeledTask a) -> Task (ProcessReference a) | iData a
spawnProcess uid activate (label,task) = mkBasicTask "spawnProcess" spawnProcess`
where
	spawnProcess` tst=:{TSt|mainTask}
		# (curUid,tst)		= getCurrentUser tst
		# (user,tst)		= getUser uid tst
		# (delegator,tst)	= getUser curUid tst
		# (curTime,tst) 	= accHStTSt (accWorldHSt time) tst
		# (dynId,tst)		= DynamicDB@createDynamic (dynamic createDynamicTask task) tst
		# (newPid,tst)		= ProcessDB@createProcess (entry mainTask dynId curTime user delegator) tst	
		| uid == curUid
			# tst			= addNewProcess newPid tst
			= (ProcessReference newPid, {tst & activated = True})
		| otherwise
			= (ProcessReference newPid, {tst & activated = True})
				
	entry pid did now user delegator	= mkDynamicProcessEntry label did now user delegator (if activate Active Suspended) pid	// Create a process database entry
	
	//Turn a task yielding a value of type a into a value of dynamic
	createDynamicTask :: !(Task a) -> (Task Dynamic) | iData a
	createDynamicTask task = Task Nothing (createDynamicTask` task)
	where
		createDynamicTask` task tst
			= let (a, tst`) = applyTask task tst in (dynamic a, tst`)

waitForProcess :: (ProcessReference a) -> Task (Maybe a) | iData a
waitForProcess (ProcessReference pid) = mkBasicTask "waitForProcess" waitForProcess`
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


getProcessStatus :: (ProcessReference a) -> Task ProcessStatus | iData a
getProcessStatus (ProcessReference pid) = mkBasicTask "getProcessStatus" getProcessStatus`
where
	getProcessStatus` tst
		# (mbProcess,tst)	= ProcessDB@getProcess pid tst
		= case mbProcess of
			Just {Process | status}	= (status, tst)
			Nothing							= (Deleted, tst)
			
getProcessOwner :: (ProcessReference a) -> Task (Maybe Int)
getProcessOwner (ProcessReference pid) = mkBasicTask "getProcess" getProcessStatus`
where
	getProcessStatus` tst 
	# (process,tst)	= ProcessDB@getProcess pid tst
	# owner = if (isNothing process) Nothing (Just (fst (fromJust process).properties.TaskProperties.user))
	= (owner,tst)

activateProcess	:: (ProcessReference a)	-> Task Bool | iData a
activateProcess (ProcessReference pid) = mkBasicTask "activateProcess" activateProcess`
where
	activateProcess` tst = ProcessDB@setProcessStatus Active pid tst

suspendProcess :: (ProcessReference a) -> Task Bool	| iData a
suspendProcess (ProcessReference pid) = mkBasicTask "suspendProcess" suspendProcess`
where
	suspendProcess` tst	= ProcessDB@setProcessStatus Suspended pid tst

suspendCurrentProcess :: Task Bool
suspendCurrentProcess = mkBasicTask "suspendCurrentProcess" suspendCurrentProcess`
where
	suspendCurrentProcess` tst
		# (pid, tst)	= getCurrentProcess tst
		= ProcessDB@setProcessStatus Suspended pid tst
		
deleteProcess :: (ProcessReference a) -> Task Bool | iData a
deleteProcess (ProcessReference pid) = mkBasicTask "deleteProcess" deleteProcess`
where
	deleteProcess` tst = ProcessDB@deleteProcess pid tst

deleteCurrentProcess :: Task Bool
deleteCurrentProcess = mkBasicTask "deleteCurrentProcess" deleteCurrentProcess`
where
	deleteCurrentProcess` tst
		# (pid, tst)	= getCurrentProcess tst
		= ProcessDB@deleteProcess pid tst

updateProcessOwner :: UserId (ProcessReference a) ->	Task Bool | iData a 
updateProcessOwner uid (ProcessReference pid) = mkBasicTask "updateProcessOwner" setProcessOwner`
where
	setProcessOwner` tst
		# (curUid,tst)		= getCurrentUser tst
		# (user,tst)		= getUser uid tst
		# (delegator,tst)	= getUser curUid tst
		= ProcessDB@setProcessOwner user delegator pid tst


//New "meta" process tasks
getCurrentProcessId :: Task ProcessId
getCurrentProcessId = mkBasicTask "getCurrentProcessId" getCurrentProcessId`
where
	getCurrentProcessId` tst=:{staticInfo}
		= (staticInfo.currentProcessId,tst)

getProcess :: !ProcessId -> Task (Maybe Process)
getProcess pid = mkBasicTask "getProcess" (\tst -> ProcessDB@getProcess pid tst)

getProcessForUser :: !UserId !ProcessId -> Task (Maybe Process)
getProcessForUser uid pid = mkBasicTask "getProcessForUser" (\tst -> ProcessDB@getProcessForUser uid pid tst)

getProcesses :: ![ProcessStatus] -> Task [Process]
getProcesses statuses = mkBasicTask "getProcesses" (\tst -> ProcessDB@getProcesses statuses tst)

getProcessesById :: ![ProcessId] -> Task [Process]
getProcessesById ids = mkBasicTask "getProcessesById" (\tst -> ProcessDB@getProcessesById ids tst)

getProcessesForUser	:: !UserId ![ProcessStatus] Bool -> Task [Process]
getProcessesForUser uid statuses ignoreEmbedded = mkBasicTask "getProcessesForUser" (\tst -> ProcessDB@getProcessesForUser uid statuses ignoreEmbedded tst)

setProcessOwner :: !UserId !ProcessId -> Task Bool
setProcessOwner uid pid = mkBasicTask "setProcessOwner" setProcessOwner`
where
	setProcessOwner` tst
		# (cur,tst)			= getCurrentUser tst //Current user is the new delegator of the process
		# (user,tst)		= getUser uid tst
		# (delegator,tst)	= getUser cur tst
		= ProcessDB@setProcessOwner user delegator pid tst

setProcessStatus :: !ProcessStatus !ProcessId -> Task Bool
setProcessStatus status pid = mkBasicTask "setProcessStatus" (\tst -> ProcessDB@setProcessStatus status pid tst)
