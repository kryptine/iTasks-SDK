implementation module SessionDB

import StdEnv, Maybe
import StdGeneric
import TSt, Store
import Random, Time

instance SessionDB IWorld
where
	getSessions	:: !*IWorld -> (![Session], !*IWorld)
	getSessions iworld
		= sessionStore id iworld
	
	getSessionsForUser :: !User !*IWorld -> (![Session], !*IWorld)
	getSessionsForUser user iworld
		# (sessions, iworld)				= sessionStore id iworld
		= ([s \\ s <- sessions | s.Session.user == user], iworld)
	
	getSession :: !SessionId !*IWorld -> (!Maybe Session, !*IWorld)
	getSession sid iworld
		# (sessions, iworld)				= sessionStore id iworld
		= case [s \\ s <- sessions | s.Session.sessionId == sid] of
			[s]	= (Just s, iworld)
			_	= (Nothing, iworld)
	
	createSession :: !(Maybe User) !*IWorld -> (!Session,!*IWorld)
	createSession mbUser iworld=:{IWorld|timestamp}
		# (sid, iworld)		= genSessionId iworld
		# user				= case mbUser of (Just u) = u; Nothing = SessionUser sid;
		# session			= {Session | sessionId = sid, user = user, timestamp}
		# (sessions, iworld)= sessionStore (\l -> [session:l]) iworld
		= (session,iworld)
	
	restoreSession	:: !SessionId !*IWorld -> (!Maybe Session, !Bool, !*IWorld)
	restoreSession sid iworld =:{IWorld|timestamp}
		# (sessions, iworld)			= sessionStore id iworld
		# (mbSession, before, after)	= findSession sid [] sessions
		= case mbSession of
			Nothing
				= (Nothing, False, iworld)												// Not found and no timeout
			Just s
				| (diffTime timestamp s.Session.timestamp) > iworld.config.sessionTime	// Session found but timed out
					# (_, iworld)	= sessionStore (\_ -> (before ++ after)) iworld
					= (Nothing, True, iworld)
				| otherwise																// Session found and still valid
					# (_, iworld)	= sessionStore (\_ -> (before ++ [{Session|s & timestamp = timestamp}: after])) iworld
				 	= (Just s, False, iworld)
		
	deleteSession	:: !SessionId !*IWorld -> (!Bool,!*IWorld)
	deleteSession sid iworld
		# (sessions, iworld)		= sessionStore id iworld
		# (mbSession, before, after)= findSession sid [] sessions
		| isJust mbSession
			# (_, iworld)			= sessionStore (\_ -> (before ++ after)) iworld
			= (True,iworld)
		| otherwise
			= (False, iworld)
	
findSession :: !String ![Session] ![Session] -> (!Maybe Session, ![Session], ![Session]) 
findSession sid before [] = (Nothing, reverse before, [])
findSession sid before [s=:{sessionId}:after]
	| sid == sessionId	= (Just s, reverse before, after)
						= findSession sid [s:before] after
						
genSessionId :: !*IWorld -> (!String, !*IWorld)
genSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) , {IWorld|iworld & world = world})

sessionStore :: !([Session] -> [Session]) !*IWorld -> (![Session],!*IWorld) 
sessionStore fn iworld
	# (mbList,iworld)	= loadValue "SessionDB" iworld
	# list 				= fn (case mbList of Nothing = []; Just list = list)
	# iworld			= storeValue "SessionDB" list iworld
	= (list,iworld)

instance SessionDB TSt
where
	getSessions	:: !*TSt -> (![Session], !*TSt)
	getSessions tst = accIWorldTSt getSessions tst
	getSessionsForUser :: !User !*TSt -> (![Session], !*TSt)
	getSessionsForUser user tst = accIWorldTSt (getSessionsForUser user) tst
	getSession :: !SessionId !*TSt -> (!Maybe Session, !*TSt)
	getSession sessionId tst = accIWorldTSt (getSession sessionId) tst
	createSession :: !(Maybe User) !*TSt -> (!Session,!*TSt)
	createSession user tst = accIWorldTSt (createSession user) tst
	restoreSession :: !SessionId !*TSt -> (!Maybe Session, !Bool, !*TSt)
	restoreSession sessionId tst=:{TSt|iworld}
		# (mbSession,timeout,iworld) = restoreSession sessionId iworld
		= (mbSession,timeout,{TSt|tst & iworld = iworld})
	deleteSession :: !SessionId !*TSt -> (!Bool,!*TSt)
	deleteSession sessionId tst = accIWorldTSt (deleteSession sessionId) tst