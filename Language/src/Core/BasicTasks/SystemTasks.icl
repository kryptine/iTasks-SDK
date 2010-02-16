implementation module SystemTasks


from TSt import :: Task, :: TSt(..), :: Store, :: HTTPRequest, :: Config
from TSt import :: ChangeLifeTime, :: StaticInfo(..), :: Options, :: Workflow
from TSt import mkInstantTask, mkMonitorTask
from TSt import accWorldTSt, loadProcessResult, taskLabel, taskNrFromString
from TSt import qualified createTaskInstance

import Types
from TaskTree import :: TaskTree, :: TaskInfo, ::TaskProperties, :: TaskManagerProperties(..), :: TaskPriority(..)

from Time	import :: Timestamp

from UserDB	import qualified getUser

from ProcessDB import :: Process{..}, :: ProcessStatus(..)
from ProcessDB import qualified class ProcessDB(..)
from ProcessDB import qualified instance ProcessDB TSt

from DynamicDB import :: DynamicId
from DynamicDB import qualified class DynamicDB(..)
from DynamicDB import qualified instance DynamicDB TSt

from	iTasks import class iTask
import	GenPrint, GenParse, GenVisualize, GenUpdate

getCurrentUser :: Task User
getCurrentUser = mkInstantTask "getCurrentUserId" getCurrentUser`
where
	getCurrentUser` tst=:{staticInfo}
		= (staticInfo.currentSession.user,tst)
		
getCurrentProcessId :: Task ProcessId
getCurrentProcessId = mkInstantTask "getCurrentProcessId" getCurrentProcessId`
where
	getCurrentProcessId` tst=:{staticInfo}
		= (staticInfo.currentProcessId,tst)
		
getDefaultValue :: Task a | iTask a
getDefaultValue = mkInstantTask "getDefaultValue" getDefaultValue`
where
	getDefaultValue` tst
		= accWorldTSt defaultValue tst

//BUG: username is not used what's happening here?		
spawnProcess :: !UserName !Bool !(Task a) -> Task (ProcessRef a) | iTask a
spawnProcess username activate task = mkInstantTask "spawnProcess" spawnProcess`
where
	spawnProcess` tst=:{TSt|mainTask,staticInfo}
		# user			= staticInfo.currentSession.user
		# properties	=
			{ TaskManagerProperties
			| worker	= (user.User.userName,user.User.displayName)
			, subject 	= taskLabel task
			, priority	= NormalPriority
			, deadline	= Nothing
			}
		# (pid,tst)			= TSt@createTaskInstance task properties True tst
		= (ProcessRef pid, {tst & activated = True})
	
waitForProcess :: (ProcessRef a) -> Task (Maybe a) | iTask a
waitForProcess (ProcessRef pid) = mkMonitorTask "waitForProcess" waitForProcess`
where
	waitForProcess` tst 
		# (mbProcess,tst) = ProcessDB@getProcess pid tst
		= case mbProcess of
			Just {Process | processId, status}
				= case status of
					Finished
						# (mbResult,tst)			= loadProcessResult (taskNrFromString pid) tst
						= (mbResult,{tst & activated = True})
					_	
						= (Nothing, {tst & activated = False})	// We are not done yet...
			_	
				= (Nothing, {tst & activated = True})	//We could not find the process in our database, we are done
