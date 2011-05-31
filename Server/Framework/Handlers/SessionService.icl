implementation module SessionService

import StdBool, StdList
import HTTP, Text, JSON
import Types, SessionDB, HtmlUtil
from UserDB import qualified class UserDB(..), instance UserDB IWorld

sessionService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)
sessionService url format path req iworld
	# html = format == "html"
	= case path of
		//List all sessions that you are allowed to view
		//The root user can view all sessions and any other user can view sessions owned by him/her
		[]
			# (mbSession,iworld)	= restoreSession sessionParam iworld
			| isError mbSession
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromError mbSession))]
				= (serviceResponse html "Session list" listDetails url listParams json, iworld)
			
			# session = fromOk mbSession	
			# (sessions,iworld)	= case session.Session.user of
				RootUser
					| userParam == ""
						= getSessions iworld
					| otherwise
						= getSessionsForUser (NamedUser userParam) iworld
				user
					= getSessionsForUser user iworld
					
			# json = JSONObject [("success",JSONBool True),("sessions", toJSON sessions)]
			= (serviceResponse html "Session list" listDetails url listParams json, iworld)
				
		//Create a new session
		["create"]			
			//Anonymous session
			| usernameParam == "" && passwordParam == ""
				# (session, iworld)	= createSession Nothing iworld
				# json				= JSONObject [("success",JSONBool True),("session",toJSON session)]
				= (serviceResponse html "Create session" createDescription url createParams json, iworld)
			//Authenticated session
			| otherwise
				# (mbUser, iworld) = 'UserDB'.authenticateUser usernameParam passwordParam iworld
				= case mbUser of
					Just user
						# (session, iworld)	= createSession (Just user) iworld
						# json				= JSONObject [("success",JSONBool True),("session",toJSON session)]
						= (serviceResponse html "Create session" createDescription url createParams json, iworld)
					Nothing
						# json	= JSONObject [("success",JSONBool False),("error",JSONString "Incorrect username or password")]
						= (serviceResponse html "Create session" createDescription url createParams json, iworld)
			
		//Show details of an existing sessions
		[sessionId]		
			# (mbSession, iworld) = getSession sessionId iworld
			= case mbSession of
				Just session
					# json		= JSONObject [("success",JSONBool True),("session",toJSON session)]
					= (serviceResponse html "Session details" detailsDescription url [] json, iworld)
				Nothing
					= (notFoundResponse req, iworld)
			
		//Destroy an existing session
		[sessionId,"delete"]
			# (deleted,iworld)	= deleteSession sessionId iworld
			# json			= JSONObject [("success", JSONBool True)]
			| deleted
				= (serviceResponse html "Delete session" deleteDescription url [] json, iworld)
			| otherwise
				= (notFoundResponse req, iworld)
				
		_		= (notFoundResponse req, iworld)
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