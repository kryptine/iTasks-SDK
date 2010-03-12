implementation module UserListHandler

import StdList
import Http, TSt
import UserDB
import JSON

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
	# (users,tst) = getUsers tst
	= ({http_emptyResponse & rsp_data = toJSON
		{UserResponse| users = [{UserRecord
								|user = user.User.displayName +++ " <" +++ user.User.userName +++ ">"
								} \\ user <- users], total = length users}}, tst)