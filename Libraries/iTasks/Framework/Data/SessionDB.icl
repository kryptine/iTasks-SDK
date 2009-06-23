implementation module SessionDB

import StdEnv, StdMaybe
import StdGeneric, GenBimap
import HSt,TSt, iDataFormlib

import Time
import MersenneTwister

SESSION_TIMEOUT :== 1800 // Half-hour sessions

derive gForm Session
derive gUpd Session
derive gPrint Session
derive gParse Session

instance SessionDB HSt
where
	createSession	:: !Int ![String] !*HSt	-> (!Session,!*HSt)
	createSession uid roles hst
		# (sid, hst)		= genSessionId hst
		# (ts, hst)			= getTimeStamp hst
		# session			= {Session | sessionId = sid, userId = uid, roles = roles, timestamp = ts}
		# (sessions, hst)	= sessionStore (\l -> [session:l]) hst
		= (session,hst)
			
	restoreSession	:: !String !*HSt -> (!Maybe Session, !Bool, !*HSt)
	restoreSession sid hst 
		# (sessions, hst)				= sessionStore id hst
		# (ts, hst)						= getTimeStamp hst
		# (mbSession, before, after)	= findSession sid [] sessions
		= case mbSession of
			Nothing
				= (Nothing, False, hst)					//Not found and no timeout
			Just s
				| (ts - s.timestamp) > SESSION_TIMEOUT	//Session found but timed out
					# (_, hst)	= sessionStore (\_ -> (before ++ after)) hst
					= (Nothing, True, hst)
				| otherwise								//Session found and still valid
					# (_, hst)	= sessionStore (\_ -> (before ++ [{s & timestamp = ts}: after])) hst
				 	= (Just s, False, hst)
		
	destroySession	:: !String !*HSt -> *HSt
	destroySession sid hst
		# (sessions, hst)		= sessionStore id hst
		# (_, before, after)	= findSession sid [] sessions
		# (_, hst)				= sessionStore (\_ -> (before ++ after)) hst
		= hst
	
findSession :: !String ![Session] ![Session] -> (!Maybe Session, ![Session], ![Session]) 
findSession sid before [] = (Nothing, reverse before, [])
findSession sid before [s=:{sessionId}:after]
	| sid == sessionId	= (Just s, reverse before, after)
						= findSession sid [s:before] after
						
genSessionId :: !*HSt -> (!String, !*HSt)
genSessionId hst
	# (Clock seed, hst)	= accWorldHSt clock hst
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt seed]) ,hst)

getTimeStamp :: !*HSt -> (!Int, !*HSt)
getTimeStamp hst
	# (Time t, hst) = accWorldHSt time hst
	= (t, hst)

sessionStore ::  !([Session] -> [Session]) !*HSt -> (![Session],!*HSt) 
sessionStore fn hst		
	# (form,hst) = mkStoreForm (Init, pFormId "SessionDB" []) fn hst
	= (form.Form.value, hst)
	
instance SessionDB TSt
where
	createSession uid roles tst		= accHStTSt (createSession uid roles) tst
	restoreSession sid tst=:{hst}	= let (mbs,to,hst`) = restoreSession sid hst in (mbs,to,{tst&hst=hst`})
	destroySession sid tst			= appHStTSt (destroySession sid) tst