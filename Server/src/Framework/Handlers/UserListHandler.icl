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
handleUserListRequest req tst
	# (users,tst)	= getUsers tst
	# filtered		= [{UserRecord
					   |user = fromUserName (toUserName user)
					   } \\ user <- users
					   | query == "" || startsWith query user.User.userName || startsWith query user.User.displayName
					  ]
	= ({http_emptyResponse & rsp_data = toJSON {UserResponse| users = filtered, total = length filtered}}, tst)
where
	query = http_getValue "query" req.arg_post ""