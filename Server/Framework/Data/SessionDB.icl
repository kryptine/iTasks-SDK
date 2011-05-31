implementation module SessionDB

import StdEnv, Maybe
import StdGeneric
import Types, Config, Store, Random, Time, Error

instance SessionDB IWorld
where
	getSessions	:: !*IWorld -> (![Session], !*IWorld)
	getSessions iworld
		= readSessionStore iworld
	
	getSessionsForUser :: !User !*IWorld -> (![Session], !*IWorld)
	getSessionsForUser user iworld
		# (sessions, iworld)				= readSessionStore iworld
		= ([s \\ s <- sessions | s.Session.user == user], iworld)
	
	getSession :: !SessionId !*IWorld -> (!Maybe Session, !*IWorld)
	getSession sid iworld
		# (sessions, iworld)				= readSessionStore iworld
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
	
	restoreSession	:: !SessionId !*IWorld -> (!MaybeErrorString Session,!*IWorld)
	restoreSession sid iworld =:{IWorld|timestamp}
		# (sessions, iworld)			= readSessionStore iworld
		# (mbSession, before, after)	= findSession sid [] sessions
		= case mbSession of
			Nothing
				= (Error "Failed to load session", iworld)												// Not found and no timeout
			Just s
				| (diffTime timestamp s.Session.timestamp) > iworld.config.sessionTime	// Session found but timed out
					# (_, iworld)	= sessionStore (\_ -> (before ++ after)) iworld
					= (Error "Session timed out", iworld)
				| otherwise																// Session found and still valid
					# (_, iworld)	= sessionStore (\_ -> (before ++ [{Session|s & timestamp = timestamp}: after])) iworld
				 	= (Ok s, iworld)
		
	deleteSession :: !SessionId !*IWorld -> (!Bool,!*IWorld)
	deleteSession sid iworld
		# (sessions, iworld)		= readSessionStore iworld
		# (mbSession, before, after)= findSession sid [] sessions
		| isJust mbSession
			# (_, iworld)			= sessionStore (\_ -> (before ++ after)) iworld
			= (True,iworld)
		| otherwise
			= (False, iworld)
			
	lastChange :: !*IWorld -> (!Timestamp,!*IWorld)
	lastChange iworld
		# (mbTs,iworld) = getStoreTimestamp "SessionDB" iworld
		= (fromMaybe (Timestamp 0) mbTs,iworld)
	
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
	
readSessionStore :: !*IWorld -> (![Session],!*IWorld) 
readSessionStore iworld
	# (mbList,iworld)	= loadValue "SessionDB" iworld
	= (fromMaybe [] mbList,iworld)
