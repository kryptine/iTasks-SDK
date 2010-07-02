implementation module SessionDBTasks

import StdTuple
from Types		import :: SessionId, :: Session, :: User

from SessionDB	import qualified class SessionDB(..)
from SessionDB	import qualified instance SessionDB TSt

from TSt		import :: TSt, :: Task
from TSt		import mkInstantTask, mkTaskFunction


from	iTasks import class iTask
import	GenPrint, GenParse, GenVisualize, GenUpdate

createSession :: !User -> Task Session
createSession user = mkInstantTask "createSession" (mkTaskFunction ('SessionDB'.createSession user))

destroySession :: !SessionId -> Task Void
destroySession sessionId = mkInstantTask "destroySession" (\tst -> (TaskFinished Void, snd ('SessionDB'.deleteSession sessionId tst)))