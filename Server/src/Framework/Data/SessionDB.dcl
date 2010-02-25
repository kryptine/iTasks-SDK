definition module SessionDB
/**
* This module provides the iTasks session database. It provides 
* functions for creating and restoring user sessions.
*/
import StdMaybe
import TSt

/**
* Create a new session
* 
* @param the user to create the session for
*/
createSession	:: !User !*TSt	-> (!Session,!*TSt)
/**
* Try to restore an existing session
*
* @param session id
*
* @return session if found
* @return whether a session timeout occurred
*/
restoreSession	:: !String !*TSt -> (!Maybe Session, !Bool, !*TSt)
/**
* Explicitly destroy an existing session.
*
* @param session id
*/
destroySession	:: !String !*TSt -> *TSt