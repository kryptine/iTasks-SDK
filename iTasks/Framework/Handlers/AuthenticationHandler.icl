implementation module AuthenticationHandler

import Http
import HSt
import Session
import StdEnv
import StdMaybe

handleAuthenticationRequest :: !HTTPRequest *HSt -> (!HTTPResponse, !*HSt)
handleAuthenticationRequest req hst
	= case getUserInfo (get "username" req.arg_post) (get "password" req.arg_post) of
		Just (uid, roles, displayName)
			# (session, hst =:{states, world})	= createSession uid roles hst
			# (states, world) = storeServerStates states world
			= ({http_emptyResponse & rsp_data = encodeSuccess session.sessionId displayName},{hst & states = states, world = world})
		Nothing
			= ({http_emptyResponse & rsp_data = encodeFailure},hst)
where
	get key []		= Nothing
	get key [(x1,x2):xs]
		| key == x1	= Just x2
					= get key xs
	
	encodeFailure	= "{\"success\": false,\" error\": \"Incorrect username or password\"}"
	encodeSuccess sid displayName	= "{\"success\": true, \"displayName\": \"" +++ displayName +++ "\", sessionId: \"" +++ sid +++ "\"}"

	//Hardcoded users
	getUserInfo (Just username) (Just password) 	= getUserInfo` username password
	getUserInfo _				_					= Nothing
	
	getUserInfo` "president" _						= Just (1, ["president"], "Organization President")
	getUserInfo` "manager" _						= Just (2, ["manager"], "Middle Manager")
	getUserInfo` "worker1" _						= Just (3, ["worker"], "Office Worker 1")
	getUserInfo` "worker2" _						= Just (4, ["worker"], "Office Worker 2")
	getUserInfo` _ _								= Nothing
	
	