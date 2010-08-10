implementation module UserService

import Http, TSt
import HtmlUtil
import UserDB
import StdOrdList

derive JSONEncode UserDetails, Password

userService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
userService url html path req tst
	# (mbSessionErr,tst)	= initSession sessionParam tst
	# (session,tst)			= getCurrentSession tst
	= case path of
		//List users
		[]
			| isJust mbSessionErr
				= (serviceResponse html "users" url params (jsonSessionErr mbSessionErr), tst)	
			
			# (users,tst)	= getUsers tst
			# items			= [details \\ RegisteredUser details <- users]
			# json			= JSONObject [("success",JSONBool True),("users",toJSON items)]
			= (serviceResponse html "users" url params json, tst)
		//List usernames
		["names"]
			| isJust mbSessionErr
				= (serviceResponse html "users names" url params (jsonSessionErr mbSessionErr), tst)	
			# (users,tst)	= getUsers tst
			# json			= JSONObject [("success",JSONBool True),("users",toJSON (sort [toString u \\ u <- users]))]
			= (serviceResponse html "users names" url params json, tst)
		//Show user details
		[userId]
			| isJust mbSessionErr
				= (serviceResponse html "user details" url params (jsonSessionErr mbSessionErr), tst)
			# (mbUser,tst)	= getUser userId tst
			= case mbUser of
				Just (RegisteredUser details)
					# json	= JSONObject [("success",JSONBool True),("user",toJSON details)]
					= (serviceResponse html "user details" url params json, tst)
				_
					= (notFoundResponse req,tst)
		_
			= (notFoundResponse req,tst)
where
	sessionParam	= paramValue "session" req
	params			= [("session",sessionParam,True)]
	
	jsonSessionErr (Just error)
					= JSONObject [("success",JSONBool False),("error", JSONString error)]