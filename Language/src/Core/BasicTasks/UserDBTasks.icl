implementation module UserDBTasks

from TSt import :: Task, :: TSt
from TSt import mkInstantTask, mkTaskFunction
import StdList, StdMaybe

from UserDB import :: User
from UserDB import qualified class UserDB(..)
from UserDB import qualified instance UserDB TSt

import InteractionTasks, CoreCombinators

getUser :: !UserId -> Task (Maybe User)
getUser username = mkInstantTask "getUser" (mkTaskFunction ('UserDB'.getUser username))

getUsers :: Task [User]
getUsers = mkInstantTask "getUsers" (mkTaskFunction 'UserDB'.getUsers)

getUsersWithRole :: !Role	-> Task [User]
getUsersWithRole role = mkInstantTask "getUsersWithRole" (mkTaskFunction ('UserDB'.getUsersWithRole role))

authenticateUser :: !String !String	-> Task (Maybe User)
authenticateUser username password = mkInstantTask "authenticateUser" (mkTaskFunction ('UserDB'.authenticateUser username password))

createUser :: !UserDetails -> Task User
createUser user = mkInstantTask "createUser" (mkTaskFunction ('UserDB'.createUser user))

updateUser :: !User !UserDetails -> Task User
updateUser user details = mkInstantTask "updateUser" (mkTaskFunction ('UserDB'.updateUser user details))

deleteUser :: !User -> Task User
deleteUser user = mkInstantTask "deleteUser" (mkTaskFunction ('UserDB'.deleteUser user))

chooseUser :: !question -> Task User | html question
chooseUser question
	= 				getUsers
	>>= \users ->	enterChoice "Choose user" question users

	
chooseUserWithRole :: !question !String -> Task User | html question
chooseUserWithRole question role
	= 				getUsersWithRole role
	>>= \users ->	enterChoice "Choose user (with role)" question users
