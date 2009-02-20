definition module SessionDB

import StdMaybe
import HSt

:: Session	= { sessionId	::	!String
			  , userId		::	!Int
			  , roles		::	![String]
			  , timestamp	::	!Int
			  }
			  
class SessionDB st
where	
	/**
	* Create a new session
	* 
	* @param user id
	* @param roles
	*/
	createSession	:: !Int ![String] !*st	-> (!Session,!*st)
	/**
	* Try to restore an existing session
	*
	* @param session id
	*
	* @return session if found
	* @return whether a session timeout occurred
	*/
	restoreSession	:: !String !*st -> (!Maybe Session, !Bool, !*st)
	/**
	* Explicitly destroy an existing session.
	*
	* @param session id
	*/
	destroySession	:: !String !*st -> *st
	
instance SessionDB HSt