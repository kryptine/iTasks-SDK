implementation module Session

import StdMaybe
import HSt

createSession	:: !Int ![String] !*HSt	-> (!Session,!*HSt)
createSession uid roles hst
	= ({sessionId = "DUMMY", userId = uid, roles = roles, timestamp = 0},hst)
	
restoreSession	:: !String !*HSt -> (!Maybe Session, !*HSt)
restoreSession sid hst = (Nothing, hst)