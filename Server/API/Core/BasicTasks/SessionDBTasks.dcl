definition module SessionDBTasks
/*
* This module provides means to create or destroy sessions
*/
from TSt	import :: Task
from Types	import :: SessionId, :: Session, :: User
import GenVisualize, GenUpdate

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