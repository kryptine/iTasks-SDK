definition module SessionDB
/**
* This module provides the iTasks session database. It provides 
* functions for creating and restoring user sessions.
*/
import StdMaybe
import TSt

/**
* List all sessions
*/
getSessions		:: !*TSt -> (![Session], !*TSt)

/**
* List all sessions for a specific user
*/
getSessionsForUser :: !User !*TSt -> (![Session], !*TSt)

/**
* Find a session in the session database
*/
getSession		:: !SessionId !*TSt -> (!Maybe Session, !*TSt)

/**
* Create a new session
* 
* @param the user to create the session for
*/
createSession	:: !User !*TSt	-> (!Session,!*TSt)

/**
* Restore an existing session
*
* @param session id
*
* @return session if found
* @return whether a session timeout occurred
*/
restoreSession	:: !SessionId !*TSt -> (!Maybe Session, !Bool, !*TSt)

/**
* Explicitly destroy an existing session.
*
* @param session id
*/
deleteSession	:: !SessionId !*TSt -> (!Bool, !*TSt)