definition module SessionDB
/**
* This module provides the iTasks session database. It provides 
* functions for creating and restoring user sessions.
*/
import Maybe, TSt

class SessionDB st
where
	/**
	* List all sessions
	*/
	getSessions		:: !*st -> (![Session], !*st)
	/**
	* List all sessions for a specific user
	*/
	getSessionsForUser :: !User !*st -> (![Session], !*st)	
	/**
	* Find a session in the session database
	*/
	getSession		:: !SessionId !*st -> (!Maybe Session, !*st)	
	/**
	* Create a new session
	* 
	* @param the user to create the session for. When Nothing is used an anonymous session is created.
	*/
	createSession	:: !(Maybe User) !*st	-> (!Session,!*st)	
	/**
	* Restore an existing session
	*
	* @param session id
	*
	* @return session if found
	* @return whether a session timeout occurred
	*/
	restoreSession	:: !SessionId !*st -> (!Maybe Session, !Bool, !*st)	
	/**
	* Explicitly destroy an existing session.
	*
	* @param session id
	*/
	deleteSession	:: !SessionId !*st -> (!Bool, !*st)
	/**
	* Gets the timestamp of the last change of the session database.
	*
	* @param A unique database handle
	*
	* @return The timestamp
	* @retrun The database handle 
	*/
	lastChange :: !*st -> (!Timestamp,!*st)
	
instance SessionDB IWorld
instance SessionDB TSt