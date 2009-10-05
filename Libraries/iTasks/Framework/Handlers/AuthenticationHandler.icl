implementation module AuthenticationHandler

import StdEnv, StdMaybe
import Http
import SessionDB, UserDB

handleAuthenticationRequest :: !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
handleAuthenticationRequest req tst
	# (mbUser, tst) = authenticateUser username password tst
	= case mbUser of
		Just user
			# (session, tst)	= createSession user tst
			# tst				= flushStore tst
			= ({http_emptyResponse & rsp_data = encodeSuccess session.sessionId user.displayName},tst)
		Nothing
			= ({http_emptyResponse & rsp_data = encodeFailure},tst)
where
	username = http_getValue "username" req.arg_post ""
	password = http_getValue "password" req.arg_post ""
	
	encodeFailure	= "{\"success\": false, \"error\": \"Incorrect username or password\"}"
	encodeSuccess sid displayName	= "{\"success\": true, \"displayName\": \"" +++ displayName +++ "\", \"sessionId\": \"" +++ sid +++ "\"}"