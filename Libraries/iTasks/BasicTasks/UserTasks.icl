implementation module UserTasks

from TSt import :: Task, :: TSt
from TSt import mkInstantTask
from TSt import qualified getCurrentUser
import StdList
from UserDB import qualified class UserDB (..)
from UserDB import qualified instance UserDB TSt

import InteractionTasks, CoreCombinators

getCurrentUser :: Task (UserId, String)
getCurrentUser = mkInstantTask "getCurrentUserId" getCurrentUser`
where
	getCurrentUser` tst
		# (cur,tst)	= TSt@getCurrentUser tst
		= UserDB@getUser cur tst

getUser :: !UserId -> Task (UserId,String)
getUser uid = mkInstantTask "getUser" (UserDB@getUser uid)

getUsers :: Task [(UserId,String)]
getUsers = mkInstantTask "getUsers" UserDB@getUsers

getUsersWithRole :: !String	-> Task [(UserId,String)]
getUsersWithRole role = mkInstantTask "getUsersWithRole" (UserDB@getUsersWithRole role)
	
getDisplayNames :: ![UserId] -> Task [String]
getDisplayNames uids = mkInstantTask "getDisplayNames" (UserDB@getDisplayNames uids)

getUserNames :: ![UserId] -> Task [String]
getUserNames uids = mkInstantTask "getUserNames" (UserDB@getUserNames uids)

getRoles :: ![UserId]	-> Task [[String]]
getRoles uids = mkInstantTask "getRoles" (UserDB@getRoles uids)
	
chooseUser :: !question -> Task (UserId,String) | html question
chooseUser question
	= 				getUsers
	>>= \users ->	enterChoice question users

	
chooseUserWithRole :: !question !String -> Task (UserId,String) | html question
chooseUserWithRole question role
	= 				getUsersWithRole role
	>>= \users ->	enterChoice question users
