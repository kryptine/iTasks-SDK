implementation module SessionDBTasks

from SessionDB	import :: Session
from SessionDB	import qualified createSession
from SessionDB	import qualified destroySession

from UserDB		import :: User

from TSt		import :: TSt, :: Task
from TSt		import mkInstantTask

from iTasks import class iTask
import GenPrint, GenParse, GenVisualize, GenUpdate

createSession :: !User -> Task Session
createSession user = mkInstantTask "createSession" (SessionDB@createSession user)

destroySession :: !String -> Task Void
destroySession sessionId = mkInstantTask "destroySession" (\tst -> (Void, SessionDB@destroySession sessionId tst))