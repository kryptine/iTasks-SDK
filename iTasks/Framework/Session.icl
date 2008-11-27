implementation module Session

import StdEnv
import StdMaybe
import StdGeneric, GenBimap
import TSt, HSt, iDataFormlib

import StdDebug
import Time
import MersenneTwister //Pseudo-random generator

//TODOS:
// = Update timestamp when retrieving a session
// = Check the validity of the timestamp
// = Implement destroy session

derive gForm Session, []
derive gUpd Session, []
derive gPrint Session
derive gParse Session

createSession	:: !Int ![String] !*HSt	-> (!Session,!*HSt)
createSession uid roles hst
	# (sid, hst)		= genSessionId hst
	# (ts, hst)			= getTimeStamp hst
	#  session			= {Session | sessionId = sid, userId = uid, roles = roles, timestamp = ts}
	# (sessions, hst)	= sessionStore (\l -> [session:l]) hst
	= (session,hst)
	
restoreSession	:: !String !*HSt -> (!Maybe Session, !*HSt)
restoreSession sid hst 
	# (sessions, hst)	= sessionStore id hst
	# (ts, hst)			= getTimeStamp hst
	= (findSession sid sessions, hst)
where
	findSession sid [] = Nothing
	findSession sid [s=:{sessionId}:ss]
		| sid == sessionId	= Just s
							= findSession sid ss
	
	
destroySession	:: !String !*HSt -> *HSt
destroySession sid hst = hst


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
	# (form,hst) = mkStoreForm (Init, pFormId "SessionTable" []) fn hst
	= (form.Form.value, hst)