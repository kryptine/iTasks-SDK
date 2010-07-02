implementation module SessionDB

import StdEnv, StdMaybe
import StdGeneric
import TSt, Store

import Time
import Random

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
	
	createSession :: !User !*IWorld -> (!Session,!*IWorld)
	createSession user iworld
		# (sid, iworld)		= genSessionId iworld
		# (ts, iworld)		= getTimeStamp iworld
		# session			= {Session | sessionId = sid, user = user, timestamp = ts}
		# (sessions, iworld)= sessionStore (\l -> [session:l]) iworld
		= (session,iworld)
	
	restoreSession	:: !SessionId !*IWorld -> (!Maybe Session, !Bool, !*IWorld)
	restoreSession sid iworld 
		# (sessions, iworld)			= sessionStore id iworld
		# (ts, iworld)					= getTimeStamp iworld
		# (mbSession, before, after)	= findSession sid [] sessions
		= case mbSession of
			Nothing
				= (Nothing, False, iworld)							// Not found and no timeout
			Just s
				| (ts - s.timestamp) > iworld.config.sessionTime	// Session found but timed out
					# (_, iworld)	= sessionStore (\_ -> (before ++ after)) iworld
					= (Nothing, True, iworld)
				| otherwise											// Session found and still valid
					# (_, iworld)	= sessionStore (\_ -> (before ++ [{s & timestamp = ts}: after])) iworld
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
genSessionId iworld=:{IWorld|world}
	# (Clock seed, world)	= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt seed]) , {IWorld|iworld & world = world})

getTimeStamp :: !*IWorld -> (!Int, !*IWorld)
getTimeStamp iworld=:{IWorld|world}
	# (Timestamp t, world) = time world
	= (t, {IWorld|iworld & world = world})

sessionStore :: !([Session] -> [Session]) !*IWorld -> (![Session],!*IWorld) 
sessionStore fn iworld=:{IWorld|store,world}
	# (mbList,store,world)	= loadValue "SessionDB" store world
	# list 					= fn (case mbList of Nothing = []; Just list = list)
	# store					= storeValue "SessionDB" list store 
	= (list, {IWorld|iworld & store = store, world = world })

instance SessionDB TSt
where
	getSessions	:: !*TSt -> (![Session], !*TSt)
	getSessions tst = accIWorldTSt getSessions tst
	getSessionsForUser :: !User !*TSt -> (![Session], !*TSt)
	getSessionsForUser user tst = accIWorldTSt (getSessionsForUser user) tst
	getSession :: !SessionId !*TSt -> (!Maybe Session, !*TSt)
	getSession sessionId tst = accIWorldTSt (getSession sessionId) tst
	createSession :: !User !*TSt -> (!Session,!*TSt)
	createSession user tst = accIWorldTSt (createSession user) tst
	restoreSession :: !SessionId !*TSt -> (!Maybe Session, !Bool, !*TSt)
	restoreSession sessionId tst=:{TSt|iworld}
		# (mbSession,timeout,iworld) = restoreSession sessionId iworld
		= (mbSession,timeout,{TSt|tst & iworld = iworld})
	deleteSession :: !SessionId !*TSt -> (!Bool,!*TSt)
	deleteSession sessionId tst = accIWorldTSt (deleteSession sessionId) tst