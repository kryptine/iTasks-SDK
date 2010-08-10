implementation module SessionService

import Http
import TSt, UserDB, SessionDB
import StdBool, StdList
import JSON
import HtmlUtil, Text

derive JSONEncode Session, User, UserDetails, Password

sessionService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
sessionService url html path req tst
	= case path of
		//List all sessions that you are allowed to view
		//The root user can view all sessions and any other user can view sessions owned by her
		[]
			# (mbErr,tst)		= if ( sessionParam <> "") (initSession sessionParam tst) (Nothing,tst)	
			| isJust mbErr
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbErr))]
				= (serviceResponse html "sessions" url listParams json, tst)
			# (session,tst)		= getCurrentSession tst
			# (sessions,tst)	= case session.Session.user of
				RootUser
					| userParam == ""
						= getSessions tst
					| otherwise
						= getSessionsForUser (NamedUser userParam) tst
				user
					= getSessionsForUser user tst
					
			# json = JSONObject [("success",JSONBool True),("sessions", toJSON sessions)]
			= (serviceResponse html "sessions" url listParams json, tst)
				
		//Create a new session
		["create"]			
			//Anonymous session
			| usernameParam == "" && passwordParam == ""
				# json	= JSONObject [("success",JSONBool False),("error",JSONString "Anonymous sessions not yet supported")]
				= (serviceResponse html "create session" url createParams json, tst)
			//Authenticated session
			| otherwise
				# (mbUser, tst) = authenticateUser usernameParam passwordParam tst
				= case mbUser of
					Just user
						# (session, tst)	= createSession user tst
						# tst				= flushStore tst
						# json				= JSONObject [("success",JSONBool True),("session",toJSON session)]
						= (serviceResponse html "create session" url createParams json, tst)
					Nothing
						# json	= JSONObject [("success",JSONBool False),("error",JSONString "Incorrect username or password")]
						= (serviceResponse html "create session" url createParams json, tst)
			
		//Show details of an existing sessions
		[sessionId]		
			# (mbSession, tst) = getSession sessionId tst
			= case mbSession of
				Just session
					# json		= JSONObject [("success",JSONBool True),("session",toJSON session)]
					= (serviceResponse html "session details" url [] json, tst)
				Nothing
					= (notFoundResponse req, tst)
			
		//Destroy an existing session
		[sessionId,"delete"]
			# (deleted,tst)	= deleteSession sessionId tst
			# tst			= flushStore tst
			# json			= JSONObject [("success", JSONBool True)]
			| deleted
				= (serviceResponse html "session delete" url [] json, tst)
			| otherwise
				= (notFoundResponse req, tst)
				
		_		= (notFoundResponse req, tst)
where
	listParams		= [("session",sessionParam,False),("user", userParam, False)]
	sessionParam	= paramValue "session" req
	userParam		= paramValue "user" req
	
	createParams	= [("username",usernameParam,True),("password",passwordParam,True)]
	usernameParam	= paramValue "username" req
	passwordParam	= paramValue "password" req
