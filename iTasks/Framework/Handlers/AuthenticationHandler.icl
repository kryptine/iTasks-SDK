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
			# (session, hst)	= createSession uid roles hst
			# hst				= storeStates hst
			= ({http_emptyResponse & rsp_data = encodeSuccess session.sessionId displayName},hst)
		Nothing
			= ({http_emptyResponse & rsp_data = encodeFailure},hst)
where
	get key []		= Nothing
	get key [(x1,x2):xs]
		| key == x1	= Just x2
					= get key xs
	
	encodeFailure	= "{\"success\": false, \"error\": \"Incorrect username or password\"}"
	encodeSuccess sid displayName	= "{\"success\": true, \"displayName\": \"" +++ displayName +++ "\", sessionId: \"" +++ sid +++ "\"}"

	//Hardcoded users
	getUserInfo (Just username) (Just password) 	= getUserInfo` username password
	getUserInfo _				_					= Nothing
	
	getUserInfo` "root" _							= Just (0, ["president","manager","worker"], "Root")
	getUserInfo` "president" _						= Just (1, ["president"], "Organization President")
	getUserInfo` "manager" _						= Just (2, ["manager"], "Middle Manager")
	getUserInfo` "worker1" _						= Just (3, ["worker"], "Office Worker 1")
	getUserInfo` "worker2" _						= Just (4, ["worker"], "Office Worker 2")
	
	//Additional hardcoded users for the webshop example
	getUserInfo` "customer" _						= Just (10, [], "Webshop customer")
	getUserInfo` "bank" _							= Just (11, [], "Bank authorization")
	getUserInfo` "webshop" _						= Just (12, [], "Webshop stock manager")
	getUserInfo` "mastercard" _						= Just (13, [], "Master Card authorization")
	
	getUserInfo` _ _								= Nothing
	
	