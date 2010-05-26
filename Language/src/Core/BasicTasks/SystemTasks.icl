implementation module SystemTasks

import StdList

from TSt import :: Task, :: TSt(..), :: Store, :: HTTPRequest, :: Config
from TSt import :: ChangeLifeTime, :: StaticInfo(..), :: Workflow
from TSt import mkInstantTask, mkMonitorTask
from TSt import accWorldTSt, loadProcessResult, taskLabel, taskNrFromString, setStatus
from TSt import qualified createTaskInstance

import Types
from TaskTree import :: TaskTree, :: TaskInfo, ::TaskProperties(..), :: TaskSystemProperties(..), :: TaskWorkerProperties, :: TaskManagerProperties(..), :: TaskPriority(..), ::TaskParallelType(..)

from Time	import :: Timestamp, :: Clock(..), clock
from Random	import genRandInt

from UserDB	import qualified getUser

from ProcessDB import :: Process{..}, :: ProcessStatus(..), :: Menu
from ProcessDB import qualified class ProcessDB(..)
from ProcessDB import qualified instance ProcessDB TSt

from	iTasks import class iTask
import	GenPrint, GenParse, GenVisualize, GenUpdate

getCurrentUser :: Task User
getCurrentUser = mkInstantTask "getCurrentUser" getCurrentUser`
where
	getCurrentUser` tst=:{staticInfo}
		= (TaskFinished staticInfo.currentSession.user,tst)

getCurrentProcessId :: Task ProcessId
getCurrentProcessId = mkInstantTask "getCurrentProcessId" getCurrentProcessId`
where
	getCurrentProcessId` tst=:{staticInfo}
		= (TaskFinished staticInfo.currentProcessId,tst)

getContextWorker :: Task UserName
getContextWorker = mkInstantTask "getContextWorker" getContextWorker`
where
	getContextWorker` tst=:{TSt|properties} = (TaskFinished properties.managerProps.worker,tst)

getContextManager :: Task UserName
getContextManager = mkInstantTask "getContextManager" getContextManager`
where
	getContextManager` tst=:{TSt|properties} = (TaskFinished properties.systemProps.manager, tst)

getDefaultValue :: Task a | iTask a
getDefaultValue = mkInstantTask "getDefaultValue" getDefaultValue`
where
	getDefaultValue` tst
		# (d,tst) = accWorldTSt defaultValue tst
		= (TaskFinished d,tst)


getRandomInt :: Task Int
getRandomInt = mkInstantTask "getRandomInt" getRandomInt`
where
	getRandomInt` tst
		# (Clock seed, tst)	= accWorldTSt clock tst
		= (TaskFinished (hd (genRandInt seed)), tst)
		
spawnProcess :: !UserName !Bool !(Task a) -> Task (ProcessRef a) | iTask a
spawnProcess username activate task = mkInstantTask "spawnProcess" spawnProcess`
where
	spawnProcess` tst=:{TSt|mainTask}
		# properties	=
			{ TaskManagerProperties
			| worker		 = username
			, subject 		 = taskLabel task
			, priority		 = NormalPriority
			, deadline		 = Nothing
			}
		# (result,pid,tst)	= TSt@createTaskInstance task properties True Nothing activate False tst
		= (TaskFinished (ProcessRef pid), tst)

waitForProcess :: (ProcessRef a) -> Task (Maybe a) | iTask a
waitForProcess (ProcessRef pid) = mkMonitorTask "waitForProcess" waitForProcess`
where
	waitForProcess` tst 
		# (mbProcess,tst) = ProcessDB@getProcess pid tst
		= case mbProcess of
			Just {Process | processId, status, properties}
				= case status of
					Finished
						# (mbResult,tst)					= loadProcessResult (taskNrFromString pid) tst	
						= case mbResult of
							Just (TaskFinished (a :: a^))	= (TaskFinished (Just a), tst)	
							_								= (TaskFinished Nothing, tst) //Ignore all other cases
					_	
						# tst = setStatus [Text "Waiting for result of task ",StrongTag [] [Text "\"",Text properties.managerProps.subject,Text "\""]] tst
						= (TaskBusy, tst)		// We are not done yet...
			_	
				= (TaskFinished Nothing, tst)	//We could not find the process in our database, we are done
