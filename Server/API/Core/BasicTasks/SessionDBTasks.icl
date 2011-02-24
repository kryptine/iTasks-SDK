implementation module SessionDBTasks

import StdTuple, Types, Task, TSt
from SessionDB	import qualified class SessionDB(..)
from SessionDB	import qualified instance SessionDB TSt

createSession :: !User -> Task Session
createSession user = mkInstantTask ("Create session", "Create a new session.") (mkTaskFunction ('SessionDB'.createSession (Just user)))

destroySession :: !SessionId -> Task Void
destroySession sessionId = mkInstantTask ("Destroy session", "Delete an existing session.") (\tst -> (TaskFinished Void, snd ('SessionDB'.deleteSession sessionId tst)))