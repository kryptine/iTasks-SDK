implementation module AuthenticationHandler

import Http
import StdEnv
import StdDebug

handleAuthenticationRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)
handleAuthenticationRequest req world = ({http_emptyResponse & rsp_data = if (check req) rspSuccess rspFailure}, world)
where
	check req = or (map (\(k,v) -> (k == "username" && v == "test")) req.arg_post)
	
	rspSuccess = "{success: true, uid: 42, sessionKey: '91a34bf12e97'}"
	rspFailure = "{success: false, error: 'Incorrect username or password'}"