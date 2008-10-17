implementation module AuthenticationHandler

import Http
import StdEnv
import StdMaybe

handleAuthenticationRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)
handleAuthenticationRequest req world = ({http_emptyResponse & rsp_data = encode (getUserInfo` (get "username" req.arg_post) (get "password" req.arg_post))}, world)
where
	get key []		= Nothing
	get key [(x1,x2):xs]
		| key == x1	= Just x2
					= get key xs
	
	encode (Just (uid, displayName))	= "{success: true, uid: " +++ toString uid +++ ", displayName: '" +++ displayName +++ "', sessionKey: '91a34bf12e97'}"
	encode Nothing						= "{success: false, error: 'Incorrect username or password'}"	

	//Hardcoded users
	getUserInfo "president" _						= Just (1, "Organization President")
	getUserInfo "manager" _							= Just (2, "Middle Manager")
	getUserInfo "worker1" _							= Just (3, "Office Worker 1")
	getUserInfo "worker2" _							= Just (4, "Office Worker 2")
	getUserInfo _ _									= Nothing
	
	getUserInfo` (Just username) (Just password) 	= getUserInfo username password
	getUserInfo` _				_					= Nothing