definition module iTasks.Internal.EngineTasks
/**
* This module defines the separate system tasks that the iTasks engine performs
*/
from iTasks.Internal.IWorld import :: IWorld
from iTasks.WF.Definition import :: TaskException, :: Task
from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe
from TCPIP import :: Timeout

timeout :: !(Maybe Timeout) !*IWorld -> (!Maybe Timeout,!*IWorld)

updateClock :: !*IWorld -> *(!MaybeError TaskException (), !*IWorld)

removeOutdatedSessions :: Task ()

flushWritesWhenIdle:: Task ()

stopOnStable :: !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
