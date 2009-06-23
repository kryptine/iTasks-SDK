implementation module UserTasks

from TSt import :: Task
from TSt import accHStTSt, mkBasicTask
from TSt import qualified getCurrentUser
import StdList
from UserDB import qualified class UserDB (..)
from UserDB import qualified instance UserDB TSt

import BasicCombinators, LiftingCombinators, UITasks

getCurrentUser :: Task (UserId, String)
getCurrentUser = mkBasicTask "getCurrentUserId" getCurrentUser`
where
	getCurrentUser` tst
		# (cur,tst)	= TSt@getCurrentUser tst
		= UserDB@getUser cur tst

getUser :: !UserId -> Task (UserId,String)
getUser uid = mkBasicTask "getUser" (UserDB@getUser uid)

getUsers :: Task [(UserId,String)]
getUsers = mkBasicTask "getUsers" UserDB@getUsers

getUsersWithRole :: !String	-> Task [(UserId,String)]
getUsersWithRole role = mkBasicTask "getUsersWithRole" (UserDB@getUsersWithRole role)
	
getDisplayNames :: ![UserId] -> Task [String]
getDisplayNames uids = mkBasicTask "getDisplayNames" (UserDB@getDisplayNames uids)

getUserNames :: ![UserId] -> Task [String]
getUserNames uids = mkBasicTask "getUserNames" (UserDB@getUserNames uids)

getRoles :: ![UserId]	-> Task [[String]]
getRoles uids = mkBasicTask "getRoles" (UserDB@getRoles uids)
	
chooseUser :: Task (UserId,String)
chooseUser
	= 				getUsers
	>>= \users ->	selectWithPulldown [name \\ (userId,name) <- users] 0
	>>= \chosen ->	return (users !! chosen)
	
chooseUserWithRole :: !String -> Task (UserId,String)
chooseUserWithRole role
	= 				getUsersWithRole role
	>>= \users ->	selectWithPulldown [name \\ (userId,name) <- users] 0
	>>= \chosen ->	return (users !! chosen)
