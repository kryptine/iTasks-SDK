implementation module ProcessCombinators

import StdOverloaded, StdClass, StdInt, StdArray, GenBimap
import TSt

from ProcessDB import :: Process{..}, :: ProcessStatus(..), :: StaticProcessEntry, :: DynamicProcessEntry{..}
from ProcessDB import qualified class ProcessDB(..)
from ProcessDB import qualified instance ProcessDB HSt
from ProcessDB import mkDynamicProcessEntry

from DynamicDB import qualified class DynamicDB(..)
from DynamicDB import qualified instance DynamicDB HSt

import iDataForms
import CommonCombinators

derive gForm	ProcessReference, Process, DynamicProcessEntry, StaticProcessEntry, ProcessStatus
derive gUpd		ProcessReference, Process, DynamicProcessEntry, StaticProcessEntry, ProcessStatus
derive gPrint	ProcessReference, Process, DynamicProcessEntry, StaticProcessEntry, ProcessStatus
derive gParse	ProcessReference, Process, DynamicProcessEntry, StaticProcessEntry, ProcessStatus


:: ProcessReference a 	= ProcessReference !Int		//We only keep the id in the process database

spawnProcess :: !UserId !Bool !(LabeledTask a) -> Task (ProcessReference a) | iData a
spawnProcess uid activate (label,task) = compound "spawnProcess" (Task spawnProcess`)
where
	spawnProcess` tst
		# (curUid,tst)	= getCurrentUser tst
		# (curPid,tst)	= getCurrentProcess tst
		# (dynId,tst)	= accHStTSt (DynamicDB@createDynamic (dynamic createDynamicTask task)) tst
		# (newPid,tst)	= accHStTSt (ProcessDB@createProcess (entry curPid dynId curUid)) tst
		| uid == curUid
			# tst			= addNewProcess newPid tst
			= (ProcessReference newPid, {tst & activated = True})
		| otherwise
			= (ProcessReference newPid, {tst & activated = True})
				
	entry pid did curUid	= mkDynamicProcessEntry label did uid curUid (if activate Active Suspended) pid	// Create a process database entry
	
	//Turn a task yielding a value of type a into a value of dynamic
	createDynamicTask :: !(Task a) -> (Task Dynamic) | iData a
	createDynamicTask task = Task (createDynamicTask` task)
	where
		createDynamicTask` task tst
			# (a, tst)	= accTaskTSt task tst
			# dyn		= dynamic a
			= (dyn, tst)

waitForProcess :: (ProcessReference a) -> Task (Maybe a) | iData a
waitForProcess (ProcessReference pid) = compound "waitForProcess" (Task waitForProcess`)
where
	waitForProcess` tst
		# (mbProcess,tst)	= accHStTSt (ProcessDB@getProcess pid) tst
		= case mbProcess of
			Just {Process | id, owner, status, process = RIGHT {result}}
				= case status of
					Finished
						# (mbResult,tst) = accHStTSt (DynamicDB@getDynamic result) tst
						= case mbResult of
							(Just dyn)	= (Just (unpackFinalValue dyn), {tst & activated = True}) //We are done and return the result
							Nothing		= (Nothing, {tst & activated = True}) //We could not find the result dynamic	
					_
						= (Nothing, {tst & activated = False})	// We are not done yet...
			_	= (Nothing, {tst & activated = True})	//We could not find the process in our database, we are done


	unpackFinalValue :: Dynamic -> a	| iData a
	unpackFinalValue (dynval :: a^) = dynval

getProcessStatus :: (ProcessReference a) -> Task ProcessStatus | iData a
getProcessStatus (ProcessReference pid) = compound "getProcessStatus" (Task getProcessStatus`)
where
	getProcessStatus` tst
		# (mbProcess,tst)	= accHStTSt (ProcessDB@getProcess pid) tst
		= case mbProcess of
			Just {Process | status}	= (status, tst)
			Nothing							= (Deleted, tst)
			
getProcessOwner :: (ProcessReference a) -> Task (Maybe Int)
getProcessOwner (ProcessReference pid) = compound "getProcess" (Task getProcessStatus`)
where
	getProcessStatus` tst 
	# (process,tst)	=	accHStTSt (ProcessDB@getProcess pid) tst
	# owner = if (isNothing process) Nothing (Just (fromJust process).owner)
	= (owner,tst)

activateProcess	:: (ProcessReference a)	-> Task Bool | iData a
activateProcess (ProcessReference pid) = compound "activateProcess" (Task activateProcess`)
where
	activateProcess` tst	= accHStTSt (ProcessDB@setProcessStatus Active pid) tst

suspendProcess :: (ProcessReference a) -> Task Bool	| iData a
suspendProcess (ProcessReference pid) = compound "suspendProcess" (Task suspendProcess`)
where
	suspendProcess` tst		= accHStTSt (ProcessDB@setProcessStatus Suspended pid) tst

suspendCurrentProcess :: Task Bool
suspendCurrentProcess = compound "suspendCurrentProcess" (Task suspendCurrentProcess`)
where
	suspendCurrentProcess` tst
		# (pid, tst)	= getCurrentProcess tst
		= accHStTSt (ProcessDB@setProcessStatus Suspended pid) tst
		
deleteProcess :: (ProcessReference a) -> Task Bool | iData a
deleteProcess (ProcessReference pid) = compound "deleteProcess" (Task deleteProcess`)
where
	deleteProcess` tst = accHStTSt (ProcessDB@deleteProcess pid) tst

deleteCurrentProcess :: Task Bool
deleteCurrentProcess = compound "deleteCurrentProcess" (Task deleteCurrentProcess`)
where
	deleteCurrentProcess` tst
		# (pid, tst)	= getCurrentProcess tst
		= accHStTSt (ProcessDB@deleteProcess pid) tst

updateProcessOwner :: UserId (ProcessReference a) ->	Task Bool | iData a 
updateProcessOwner uid (ProcessReference pid) = compound "updateProcessOwner" (Task setProcessOwner`)
where
	setProcessOwner` tst
		# (curUid,tst)	= getCurrentUser tst
		= accHStTSt (ProcessDB@setProcessOwner uid curUid pid) tst


//New "meta" process tasks
getCurrentProcessId :: Task ProcessId
getCurrentProcessId = mkBasicTask "getCurrentProcessId" getCurrentProcessId`
where
	getCurrentProcessId` tst=:{staticInfo}
		= (staticInfo.currentProcessId,tst)

getProcess :: !ProcessId -> Task (Maybe Process)
getProcess pid = mkBasicTask "getProcess" (\tst ->	accHStTSt (ProcessDB@getProcess pid) tst)

getProcessForUser :: !UserId !ProcessId -> Task (Maybe Process)
getProcessForUser uid pid = mkBasicTask "getProcessForUser" (\tst -> accHStTSt (ProcessDB@getProcessForUser uid pid) tst)

getProcesses :: ![ProcessStatus] -> Task [Process]
getProcesses statuses = mkBasicTask "getProcesses" (\tst -> accHStTSt (ProcessDB@getProcesses statuses) tst)

getProcessesById :: ![ProcessId] -> Task [Process]
getProcessesById ids = mkBasicTask "getProcessesById" (\tst -> accHStTSt (ProcessDB@getProcessesById ids) tst)

getProcessesForUser	:: !UserId ![ProcessStatus] -> Task [Process]
getProcessesForUser uid statuses = mkBasicTask "getProcessesForUser" (\tst -> accHStTSt (ProcessDB@getProcessesForUser uid statuses) tst)

setProcessOwner :: !UserId !ProcessId -> Task Bool
setProcessOwner uid pid = mkBasicTask "setProcessOwner" setProcessOwner`
where
	setProcessOwner` tst
		# (cur,tst)	= getCurrentUser tst //Current user is the new delegator of the process
		= accHStTSt (ProcessDB@setProcessOwner uid cur pid) tst

setProcessStatus :: !ProcessStatus !ProcessId -> Task Bool
setProcessStatus status pid = mkBasicTask "setProcessStatus" (\tst -> accHStTSt (ProcessDB@setProcessStatus status pid) tst)
