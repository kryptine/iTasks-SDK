implementation module UserDBTasks

from TSt import :: Task, :: TSt
from TSt import mkInstantTask, mkTaskFunction
import StdList, StdMaybe

from UserDB import :: User
from UserDB import qualified getUser
from UserDB import qualified getUsers
from UserDB import qualified getUsersWithRole
from UserDB import qualified getDisplayNames
from UserDB import qualified getRoles
from UserDB import qualified authenticateUser
from UserDB	import qualified createUser
from UserDB import qualified updateUser
from UserDB import qualified deleteUser

import InteractionTasks, CoreCombinators

getUser :: !UserName -> Task User
getUser username = mkInstantTask "getUser" (mkTaskFunction (UserDB@getUser username))

getUsers :: Task [User]
getUsers = mkInstantTask "getUsers" (mkTaskFunction UserDB@getUsers)

getUsersWithRole :: !Role	-> Task [User]
getUsersWithRole role = mkInstantTask "getUsersWithRole" (mkTaskFunction (UserDB@getUsersWithRole role))
	
getDisplayNames :: ![UserName] -> Task [DisplayName]
getDisplayNames usernames = mkInstantTask "getDisplayNames" (mkTaskFunction (UserDB@getDisplayNames usernames))

getRoles :: ![UserName]	-> Task [[Role]]
getRoles usernames = mkInstantTask "getRoles" (mkTaskFunction (UserDB@getRoles usernames))

authenticateUser :: !String !String	-> Task (Maybe User)
authenticateUser username password = mkInstantTask "authenticateUser" (mkTaskFunction (UserDB@authenticateUser username password))

createUser :: !User -> Task User
createUser user = mkInstantTask "createUser" (mkTaskFunction (UserDB@createUser user))

updateUser :: !User -> Task User
updateUser user = mkInstantTask "updateUser" (mkTaskFunction (UserDB@updateUser user))

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
