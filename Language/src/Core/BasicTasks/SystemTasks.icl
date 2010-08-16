implementation module SystemTasks

import StdList

from TSt import :: Task, :: TSt(..), :: IWorld(..), :: Store, :: HTTPRequest, :: Config, :: StaticInfo(..), :: Workflow
from TSt import mkInstantTask, mkMonitorTask, accWorldTSt

import Types
from TaskTree import :: TaskTree, :: TaskInfo,  :: TaskPriority(..), ::TaskParallelType(..), :: TreeType(..)
from TaskTree import :: TaskProperties(..), :: SystemProperties(..), :: WorkerProperties, :: ManagerProperties(..)

from Time	import :: Timestamp, :: Clock(..), clock
from Random	import genRandInt

from UserDB	import qualified class UserDB
from UserDB import qualified instance UserDB TSt

from ProcessDB import :: Menu

from	iTasks import class iTask
import	GenVisualize, GenUpdate

getCurrentUser :: Task User
getCurrentUser = mkInstantTask "Get current user" "Determine the currently logged in user." getCurrentUser`
where
	getCurrentUser` tst=:{staticInfo}
		= (TaskFinished staticInfo.currentSession.user,tst)

getCurrentProcessId :: Task ProcessId
getCurrentProcessId = mkInstantTask "Get current process id" "Determine the process identifier of the current task instance." getCurrentProcessId`
where
	getCurrentProcessId` tst=:{staticInfo}
		= (TaskFinished staticInfo.currentProcessId,tst)

getContextWorker :: Task User
getContextWorker = mkInstantTask "Get context worker" "Determine the worker assigned to the current task." getContextWorker`
where
	getContextWorker` tst=:{TSt|properties} = (TaskFinished properties.managerProperties.worker,tst)

getContextManager :: Task User
getContextManager = mkInstantTask "Get context manager" "Determine the manager of the current task." getContextManager`
where
	getContextManager` tst=:{TSt|properties} = (TaskFinished properties.systemProperties.manager, tst)

getDefaultValue :: Task a | iTask a
getDefaultValue = mkInstantTask "Create default value" "Create a default data value." getDefaultValue`
where
	getDefaultValue` tst=:{TSt|iworld}
		# (d,iworld)	= defaultValue iworld
		= (TaskFinished d, {TSt|tst & iworld = iworld})


getRandomInt :: Task Int
getRandomInt = mkInstantTask "Create random integer" "Create a random number." getRandomInt`
where
	getRandomInt` tst
		# (Clock seed, tst)	= accWorldTSt clock tst
		= (TaskFinished (hd (genRandInt seed)), tst)
		