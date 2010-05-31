implementation module UserDBTasks

from TSt import :: Task, :: TSt
from TSt import mkInstantTask, mkTaskFunction
import StdList, StdMaybe

from UserDB import :: User
from UserDB import qualified getUser
from UserDB import qualified getUsers
from UserDB import qualified getUsersWithRole
from UserDB import qualified authenticateUser
from UserDB	import qualified createUser
from UserDB import qualified updateUser
from UserDB import qualified deleteUser

import InteractionTasks, CoreCombinators

getUser :: !UserId -> Task User
getUser username = mkInstantTask "getUser" (mkTaskFunction (UserDB@getUser username))

getUsers :: Task [User]
getUsers = mkInstantTask "getUsers" (mkTaskFunction UserDB@getUsers)

getUsersWithRole :: !Role	-> Task [User]
getUsersWithRole role = mkInstantTask "getUsersWithRole" (mkTaskFunction (UserDB@getUsersWithRole role))

authenticateUser :: !String !String	-> Task (Maybe User)
authenticateUser username password = mkInstantTask "authenticateUser" (mkTaskFunction (UserDB@authenticateUser username password))

createUser :: !UserDetails -> Task User
createUser user = mkInstantTask "createUser" (mkTaskFunction (UserDB@createUser user))

updateUser :: !User !UserDetails -> Task User
updateUser user details = mkInstantTask "updateUser" (mkTaskFunction (UserDB@updateUser user details))

deleteUser :: !User -> Task User
deleteUser user = mkInstantTask "deleteUser" (mkTaskFunction (UserDB@deleteUser user))

chooseUser :: !question -> Task User | html question
chooseUser question
	= 				getUsers
	>>= \users ->	enterChoice question users

	
chooseUserWithRole :: !question !String -> Task User | html question
chooseUserWithRole question role
	= 				getUsersWithRole role
	>>= \users ->	enterChoice question users
