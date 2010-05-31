implementation module UserListHandler

import StdList, StdBool
import Http, TSt
import UserDB
import JSON, Text

:: UserResponse =
	{ total	:: !Int
	, users	:: ![UserRecord]
	}
:: UserRecord =
	{ user	:: !String
	}

derive JSONEncode UserResponse,UserRecord

handleUserListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleUserListRequest req tst=:{staticInfo}
	# (users,tst)	= getUsers tst
	# filtered		= (rootItem staticInfo.currentSession.Session.user) ++
	 				  [{UserRecord
					   |user = toString user
					   } \\ user =:(RegisteredUser details) <- users
					   | query == "" || startsWith query details.UserDetails.userName || startsWith query details.UserDetails.displayName
					  ]
	= ({http_emptyResponse & rsp_data = toJSON {UserResponse| users = filtered, total = length filtered}}, tst)
where
	//Only add Root user entry for root itself
	rootItem RootUser	= [{UserRecord|user = toString RootUser}]
	rootItem _			= [] 
	
	query = http_getValue "query" req.arg_post ""
	