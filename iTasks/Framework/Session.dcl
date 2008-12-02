definition module Session

import HSt
import StdMaybe

:: Session	= { sessionId	::	!String
			  , userId		::	!Int
			  , roles		::	![String]
			  , timestamp	::	!Int
			  }

/**
* Create a new session
* 
* @param user id
* @param roles
*/
createSession	:: !Int ![String] !*HSt	-> (!Session,!*HSt)

/**
* Try to restore an existing session
*
* @param session id
*
* @return session if found
* @return whether a session timeout occurred
*/
restoreSession	:: !String !*HSt -> (!Maybe Session, !Bool, !*HSt)

/**
* Explicitly destroy an existing session.
*
* @param session id
*/
destroySession	:: !String !*HSt -> *HSt