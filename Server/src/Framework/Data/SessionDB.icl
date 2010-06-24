implementation module SessionDB

import StdEnv, StdMaybe
import StdGeneric
import TSt, Store

import Time
import Random


getSessions	:: !*TSt -> (![Session], !*TSt)
getSessions tst
	= sessionStore id tst

getSessionsForUser :: !User !*TSt -> (![Session], !*TSt)
getSessionsForUser user tst
	# (sessions, tst)				= sessionStore id tst
	= ([s \\ s <- sessions | s.Session.user == user], tst)

getSession :: !SessionId !*TSt -> (!Maybe Session, !*TSt)
getSession sid tst
	# (sessions, tst)				= sessionStore id tst
	= case [s \\ s <- sessions | s.Session.sessionId == sid] of
		[s]	= (Just s, tst)
		_	= (Nothing, tst)

createSession :: !User !*TSt -> (!Session,!*TSt)
createSession user tst
	# (sid, tst)		= genSessionId tst
	# (ts, tst)			= getTimeStamp tst
	# session			= {Session | sessionId = sid, user = user, timestamp = ts}
	# (sessions, tst)	= sessionStore (\l -> [session:l]) tst
	= (session,tst)

restoreSession	:: !SessionId !*TSt -> (!Maybe Session, !Bool, !*TSt)
restoreSession sid tst 
	# (sessions, tst)				= sessionStore id tst
	# (ts, tst)						= getTimeStamp tst
	# (mbSession, before, after)	= findSession sid [] sessions
	= case mbSession of
		Nothing
			= (Nothing, False, tst)							// Not found and no timeout
		Just s
			| (ts - s.timestamp) > tst.config.sessionTime	// Session found but timed out
				# (_, tst)	= sessionStore (\_ -> (before ++ after)) tst
				= (Nothing, True, tst)
			| otherwise										// Session found and still valid
				# (_, tst)	= sessionStore (\_ -> (before ++ [{s & timestamp = ts}: after])) tst
			 	= (Just s, False, tst)
	
deleteSession	:: !SessionId !*TSt -> (!Bool,!*TSt)
deleteSession sid tst
	# (sessions, tst)		= sessionStore id tst
	# (mbSession, before, after)	= findSession sid [] sessions
	| isJust mbSession
		# (_, tst)				= sessionStore (\_ -> (before ++ after)) tst
		= (True,tst)
	| otherwise
		= (False, tst)

findSession :: !String ![Session] ![Session] -> (!Maybe Session, ![Session], ![Session]) 
findSession sid before [] = (Nothing, reverse before, [])
findSession sid before [s=:{sessionId}:after]
	| sid == sessionId	= (Just s, reverse before, after)
						= findSession sid [s:before] after
						
genSessionId :: !*TSt -> (!String, !*TSt)
genSessionId tst
	# (Clock seed, tst)	= accWorldTSt clock tst
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt seed]) ,tst)

getTimeStamp :: !*TSt -> (!Int, !*TSt)
getTimeStamp tst
	# (Timestamp t, tst) = accWorldTSt time tst
	= (t, tst)

sessionStore ::  !([Session] -> [Session]) !*TSt -> (![Session],!*TSt) 
sessionStore fn tst=:{TSt|dataStore,world = world}
	# (mbList,dstore,world)	= loadValue "SessionDB" dataStore world
	# list 					= fn (case mbList of Nothing = []; Just list = list)
	# dstore				= storeValue "SessionDB" list dstore 
	= (list, {TSt|tst & dataStore = dstore, world = world })