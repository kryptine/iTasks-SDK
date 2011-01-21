implementation module SessionDBTasks

import StdTuple, Types
from SessionDB	import qualified class SessionDB(..)
from SessionDB	import qualified instance SessionDB TSt
from TSt		import :: TSt, :: Task
from TSt		import mkInstantTask, mkTaskFunction
import GenVisualize, GenUpdate

createSession :: !User -> Task Session
createSession user = mkInstantTask ("Create session", "Create a new session.") (mkTaskFunction ('SessionDB'.createSession user))

destroySession :: !SessionId -> Task Void
destroySession sessionId = mkInstantTask ("Destroy session", "Delete an existing session.") (\tst -> (TaskFinished Void, snd ('SessionDB'.deleteSession sessionId tst)))