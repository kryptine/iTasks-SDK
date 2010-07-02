implementation module SystemTasks

import StdList

from TSt import :: Task, :: TSt(..), :: IWorld(..), :: Store, :: HTTPRequest, :: Config, :: StaticInfo(..), :: Workflow
from TSt import mkInstantTask, mkMonitorTask, accWorldTSt

import Types
from TaskTree import :: TaskTree, :: TaskInfo,  :: TaskPriority(..), ::TaskParallelType(..)
from TaskTree import :: TaskProperties(..), :: SystemProperties(..), :: WorkerProperties, :: ManagerProperties(..)

from Time	import :: Timestamp, :: Clock(..), clock
from Random	import genRandInt

from UserDB	import qualified class UserDB
from UserDB import qualified instance UserDB TSt

from ProcessDB import :: Menu

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

getContextWorker :: Task User
getContextWorker = mkInstantTask "getContextWorker" getContextWorker`
where
	getContextWorker` tst=:{TSt|properties} = (TaskFinished properties.managerProps.worker,tst)

getContextManager :: Task User
getContextManager = mkInstantTask "getContextManager" getContextManager`
where
	getContextManager` tst=:{TSt|properties} = (TaskFinished properties.systemProps.manager, tst)

getDefaultValue :: Task a | iTask a
getDefaultValue = mkInstantTask "getDefaultValue" getDefaultValue`
where
	getDefaultValue` tst=:{TSt|iworld}
		# (d,iworld)	= defaultValue iworld
		= (TaskFinished d, {TSt|tst & iworld = iworld})


getRandomInt :: Task Int
getRandomInt = mkInstantTask "getRandomInt" getRandomInt`
where
	getRandomInt` tst
		# (Clock seed, tst)	= accWorldTSt clock tst
		= (TaskFinished (hd (genRandInt seed)), tst)
		