implementation module AuthenticationHandler

import StdEnv, StdMaybe
import Http
import HSt
import SessionDB, UserDB

handleAuthenticationRequest :: !HTTPRequest *HSt -> (!HTTPResponse, !*HSt)
handleAuthenticationRequest req hst
	# (mbCredentials, hst) = authenticateUser username password hst
	= case mbCredentials of
		Just (uid,displayName,roles)
			# (session, hst)	= createSession uid roles hst
			# hst				= storeStates hst
			= ({http_emptyResponse & rsp_data = encodeSuccess session.sessionId displayName},hst)
		Nothing
			= ({http_emptyResponse & rsp_data = encodeFailure},hst)
where
	username = http_getValue "username" req.arg_post ""
	password = http_getValue "password" req.arg_post ""
	
	encodeFailure	= "{\"success\": false, \"error\": \"Incorrect username or password\"}"
	encodeSuccess sid displayName	= "{\"success\": true, \"displayName\": \"" +++ displayName +++ "\", sessionId: \"" +++ sid +++ "\"}"