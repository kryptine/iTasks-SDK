implementation module ProcessCombinators

import StdEnv, GenBimap
import TSt
import dynamic_string

from ProcessDB import :: ProcessDB, :: ProcessStatus(..), :: Process{..}, :: StaticProcessEntry, :: DynamicProcessEntry{..}
from ProcessDB import createProcess
from ProcessDB import createDynamicProcessEntry
from ProcessDB import getProcess
from ProcessDB import setProcessStatus
from ProcessDB import qualified setProcessOwner
from ProcessDB import qualified deleteProcess

import iDataForms
import CommonCombinators

derive gForm	ProcessReference, ProcessStatus
derive gUpd		ProcessReference, ProcessStatus
derive gPrint	ProcessReference, ProcessStatus
derive gParse	ProcessReference, ProcessStatus

:: ProcessReference a 	= ProcessReference !Int		//We only keep the id in the process database

spawnProcess :: !UserId !Bool !(LabeledTask a) -> Task (ProcessReference a) | iData a
spawnProcess uid activate (label,task) = newTask "spawnProcess" (Task spawnProcess`)
where
	spawnProcess` tst
		# (curUid,tst)	= getCurrentUser tst
		# (curPid,tst)	= getCurrentProcess tst
		# (newPid,tst)	= accProcessDBTSt (createProcess (entry curPid)) tst
		| uid == curUid
			# tst			= addNewProcess newPid tst
			= (ProcessReference newPid, {tst & activated = True})
		| otherwise
			= (ProcessReference newPid, {tst & activated = True})
			
	closure		= dynamic_to_string (dynamic createDynamicTask task)								// Convert the task to a dynamic task and serialize
	entry pid	= createDynamicProcessEntry label closure uid (if activate Active Suspended) pid	// Create a process database entry
	
	//Turn a task yielding a value of type a into a value of dynamic
	createDynamicTask :: !(Task a) -> (Task Dynamic) | iData a
	createDynamicTask task = Task (createDynamicTask` task)
	where
		createDynamicTask` task tst
			# (a, tst)	= accTaskTSt task tst
			# dyn		= dynamic a
			= (dyn, tst)
		

waitForProcess :: (ProcessReference a) -> Task (Maybe a) | iData a
waitForProcess (ProcessReference pid) = newTask "waitForProcess" (Task waitForProcess`)
where
	waitForProcess` tst
		# (mbProcess,tst)	= accProcessDBTSt (getProcess pid) tst
		= case mbProcess of
			Just {Process | id, owner, status, process = RIGHT {result}}
				= case status of
					Finished	
						= (Just (unpackFinalValue (string_to_dynamic {c \\ c <-: result})), {tst & activated = True})	
					_
						= (Nothing, {tst & activated = False})	// We are not done yet...
			_	= (Nothing, {tst & activated = True})	//We could not find the process in our database, we are done


	unpackFinalValue :: Dynamic -> a	| iData a
	unpackFinalValue (dynval :: a^) = dynval

getProcessStatus :: (ProcessReference a) -> Task ProcessStatus | iData a
getProcessStatus (ProcessReference pid) = newTask "getProcessStatus" (Task getProcessStatus`)
where
	getProcessStatus` tst
		# (mbProcess,tst)	= accProcessDBTSt (getProcess pid) tst
		= case mbProcess of
			Just {Process | status}	= (status, tst)
			Nothing							= (Deleted, tst)
			
activateProcess	:: (ProcessReference a)	-> Task Bool | iData a
activateProcess (ProcessReference pid) = newTask "activateProcess" (Task activateProcess`)
where
	activateProcess` tst	= accProcessDBTSt (setProcessStatus Active pid) tst

suspendProcess :: (ProcessReference a) -> Task Bool	| iData a
suspendProcess (ProcessReference pid) = newTask "suspendProcess" (Task suspendProcess`)
where
	suspendProcess` tst		= accProcessDBTSt (setProcessStatus Suspended pid) tst

suspendCurrentProcess :: Task Bool
suspendCurrentProcess = newTask "suspendCurrentProcess" (Task suspendCurrentProcess`)
where
	suspendCurrentProcess` tst
		# (pid, tst)	= getCurrentProcess tst
		= accProcessDBTSt (setProcessStatus Suspended pid) tst
		
deleteProcess :: (ProcessReference a) -> Task Bool | iData a
deleteProcess (ProcessReference pid) = newTask "deleteProcess" (Task deleteProcess`)
where
	deleteProcess` tst = accProcessDBTSt (ProcessDB@deleteProcess pid) tst

deleteCurrentProcess :: Task Bool
deleteCurrentProcess = newTask "deleteCurrentProcess" (Task deleteCurrentProcess`)
where
	deleteCurrentProcess` tst
		# (pid, tst)	= getCurrentProcess tst
		= accProcessDBTSt (ProcessDB@deleteProcess pid) tst

setProcessOwner :: UserId (ProcessReference a) ->	Task Bool | iData a 
setProcessOwner uid (ProcessReference pid) = newTask "setProcessOwner" (Task setProcessOwner`)
where
	setProcessOwner` tst = accProcessDBTSt (ProcessDB@setProcessOwner uid pid) tst

