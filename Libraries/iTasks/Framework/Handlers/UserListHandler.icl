implementation module UserListHandler

import StdList
import Http, TSt
import UserDB
import JSON

:: UserResponse =
	{ total	:: Int
	, users	:: [UserRecord]
	}
:: UserRecord =
	{ userId 		:: Int
	, displayName	:: String
	}
	
derive JSONEncode UserResponse, UserRecord

handleUserListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleUserListRequest req tst
	# (users,tst) = getUsers tst
	= ({http_emptyResponse & rsp_data = toJSON
		{UserResponse| users = [{userId = uid, displayName = dn} \\ (uid,dn) <- users], total = length users}}, tst)