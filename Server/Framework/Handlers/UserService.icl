implementation module UserService

import StdOrdList
import HTTP, Error, HtmlUtil
import SystemTypes, UserDB
from SessionDB import qualified class SessionDB(..), instance SessionDB IWorld

userService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)
userService url format path req iworld
	# html					= format == "html"
	# (mbSession,iworld)	= 'SessionDB'.restoreSession sessionParam iworld
	= case path of
		//List users
		[]
			| isError mbSession
				= (serviceResponse html "User list" listDescription url params (jsonSessionErr mbSession), iworld)	
			
			# (users,iworld)	= getUsers iworld
			# items				= [details \\ RegisteredUser details <- users]
			# json				= JSONObject [("success",JSONBool True),("users",toJSON items)]
			= (serviceResponse html "User list" listDescription url params json, iworld)
		//List usernames
		["names"]
			| isError mbSession
				= (serviceResponse html "User name list" nameListDescription url params (jsonSessionErr mbSession), iworld)	
			# (users,iworld)	= getUsers iworld
			# json				= JSONObject [("success",JSONBool True),("users",toJSON (sort [toString u \\ u <- users]))]
			= (serviceResponse html "User name list" nameListDescription url params json, iworld)
		//Show user details
		[userId]
			| isError mbSession
				= (serviceResponse html "User details" detailsDescription url params (jsonSessionErr mbSession), iworld)
			# (mbUser,iworld)	= getUser userId iworld
			= case mbUser of
				Just (RegisteredUser details)
					# json	= JSONObject [("success",JSONBool True),("user",toJSON details)]
					= (serviceResponse html "User details" detailsDescription url params json, iworld)
				_
					= (notFoundResponse req,iworld)
		_
			= (notFoundResponse req,iworld)
where
	sessionParam	= paramValue "session" req
	params			= [("session",sessionParam,True)]
	
	jsonSessionErr (Error error)
					= JSONObject [("success",JSONBool False),("error", JSONString error)]
					
listDescription		:== "This service lists the details of all users."
nameListDescription :== "This service lists the usernames of all users."
detailsDescription	:== "This service lists all information about a user."