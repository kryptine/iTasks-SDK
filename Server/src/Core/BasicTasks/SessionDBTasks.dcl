definition module SessionDBTasks

from TSt		import :: Task
from Types		import :: SessionId, :: Session, :: User

from iTasks import class iTask
import GenPrint, GenParse, GenVisualize, GenUpdate

/**
* Create a new session
*
* @param The user to create the session for
*
* @return The new session
*/
createSession :: !User -> Task Session

/**
* Destroy a session
*
* @param The session identifier
*/
destroySession :: !SessionId -> Task Void