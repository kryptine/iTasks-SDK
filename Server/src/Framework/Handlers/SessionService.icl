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
				= (serviceResponse html "Session list" listDetails url listParams json, tst)
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
			= (serviceResponse html "Session list" listDetails url listParams json, tst)
				
		//Create a new session
		["create"]			
			//Anonymous session
			| usernameParam == "" && passwordParam == ""
				# json	= JSONObject [("success",JSONBool False),("error",JSONString "Anonymous sessions not yet supported")]
				= (serviceResponse html "Create session" createDescription url createParams json, tst)
			//Authenticated session
			| otherwise
				# (mbUser, tst) = authenticateUser usernameParam passwordParam tst
				= case mbUser of
					Just user
						# (session, tst)	= createSession user tst
						# tst				= flushStore tst
						# json				= JSONObject [("success",JSONBool True),("session",toJSON session)]
						= (serviceResponse html "Create session" createDescription url createParams json, tst)
					Nothing
						# json	= JSONObject [("success",JSONBool False),("error",JSONString "Incorrect username or password")]
						= (serviceResponse html "Create session" createDescription url createParams json, tst)
			
		//Show details of an existing sessions
		[sessionId]		
			# (mbSession, tst) = getSession sessionId tst
			= case mbSession of
				Just session
					# json		= JSONObject [("success",JSONBool True),("session",toJSON session)]
					= (serviceResponse html "Session details" detailsDescription url [] json, tst)
				Nothing
					= (notFoundResponse req, tst)
			
		//Destroy an existing session
		[sessionId,"delete"]
			# (deleted,tst)	= deleteSession sessionId tst
			# tst			= flushStore tst
			# json			= JSONObject [("success", JSONBool True)]
			| deleted
				= (serviceResponse html "Delete session" deleteDescription url [] json, tst)
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

listDetails :== "This service lists the active sessions.<br />You only get this list by providing the session id of a session of the root user."
createDescription :== "This service let's you create new sessions by sending a username/password combination."
detailsDescription :== "This service provides all data of a session"
deleteDescription :== "This service deletes an existing session"