implementation module UserDBTasks

from TSt import :: Task, :: TSt
from TSt import mkInstantTask
import StdList, StdMaybe

from UserDB import :: User
from UserDB import qualified getCurrentUser
from UserDB import qualified getUser
from UserDB import qualified getUserByName
from UserDB import qualified getUsers
from UserDB import qualified getUsersWithRole
from UserDB import qualified getDisplayNames
from UserDB import qualified getUserNames
from UserDB import qualified getRoles
from UserDB import qualified authenticateUser

import InteractionTasks, CoreCombinators

getUser :: !UserId -> Task User
getUser uid = mkInstantTask "getUser" (UserDB@getUser uid)

getUserByName :: !String -> Task User
getUserByName name = mkInstantTask "getUserByName" (UserDB@getUserByName name)

getUsers :: Task [User]
getUsers = mkInstantTask "getUsers" UserDB@getUsers

getUsersWithRole :: !String	-> Task [User]
getUsersWithRole role = mkInstantTask "getUsersWithRole" (UserDB@getUsersWithRole role)
	
getDisplayNames :: ![UserId] -> Task [String]
getDisplayNames uids = mkInstantTask "getDisplayNames" (UserDB@getDisplayNames uids)

getUserNames :: ![UserId] -> Task [String]
getUserNames uids = mkInstantTask "getUserNames" (UserDB@getUserNames uids)

getRoles :: ![UserId]	-> Task [[String]]
getRoles uids = mkInstantTask "getRoles" (UserDB@getRoles uids)

authenticateUser :: !String !String	-> Task (Maybe User)
authenticateUser username password = mkInstantTask "authenticateUser" (UserDB@authenticateUser username password)

chooseUser :: !question -> Task User | html question
chooseUser question
	= 				getUsers
	>>= \users ->	enterChoice question users

	
chooseUserWithRole :: !question !String -> Task User | html question
chooseUserWithRole question role
	= 				getUsersWithRole role
	>>= \users ->	enterChoice question users
