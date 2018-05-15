definition module iTasks.Internal.EngineTasks
/**
* This module defines the separate system tasks that the iTasks engine performs
*/
from iTasks.Internal.IWorld import :: IWorld
from iTasks.WF.Definition import :: TaskException
from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe
from TCPIP import :: Timeout
from iTasks.WF.Definition import :: Task

timeout :: !(Maybe Timeout) !*IWorld -> (!Maybe Timeout,!*IWorld)

removeOutdatedSessions :: Task ()

stopOnStable :: Task ()
