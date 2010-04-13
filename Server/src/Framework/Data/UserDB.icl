implementation module UserDB

import StdEnv, StdMaybe
import StdGeneric
import TSt, Util

from Types import :: Password(..)

derive JSONEncode User, Password
derive JSONDecode User, Password
derive bimap (,), Maybe

unknownUser :: User
unknownUser = {User | userName = "unknown", displayName = "Unknown user", password = Password "", roles = []}

rootUser :: User
rootUser = {User | userName = "root", displayName = "Root", password = Password "", roles = []}

getUser :: !UserName !*TSt -> (!User,!*TSt)
getUser (UserName "root" _) tst
	= (rootUser,tst)
getUser userName tst
	# (users, tst) = userStore id tst
	= case filter (\u -> (toUserName u) == userName) users of
		[x] = (x,tst)
		_	= (unknownUser,tst)

getUserByName :: !String !*TSt -> (!User, !*TSt)
getUserByName "root" tst
	= (rootUser,tst)
getUserByName userName tst
	# (users, tst)		= userStore id tst
	= case filter (\u -> u.User.userName == userName) users of
		[x] = (x,tst)
		_	= (unknownUser,tst)
	
getUsers :: !*TSt -> (![User], !*TSt)
getUsers tst
	# (users, tst) = userStore id tst
	= (users,tst)	//Do not include the "root" user"
	
getUsersWithRole :: !String !*TSt -> (![User], !*TSt)
getUsersWithRole role tst
	# (users, tst)		= userStore id tst
	= (filter (\u -> isMember role u.User.roles) users, tst) //Do not include the "root" user"

getDisplayNames	:: ![UserName] !*TSt -> (![DisplayName], !*TSt)
getDisplayNames	usernames tst
	# (users, tst)		= userStore id tst
	= (map (displayName users) usernames, tst)
where
	displayName users (UserName "root" _) = "Root"
	displayName users name = lookupUserProperty users (\u -> u.displayName) "Unknown user" name
	
getRoles :: ![UserName] !*TSt -> (![[Role]], !*TSt)
getRoles usernames tst
	# (users, tst)		= userStore id tst
	= (map (lookupUserProperty users (\u -> u.User.roles) []) usernames, tst)


authenticateUser :: !String !String	!*TSt -> (!Maybe User, !*TSt)
authenticateUser username password tst
	| username == "root"
		| password	== tst.config.rootPassword
			= (Just rootUser, tst)
		| otherwise
			= (Nothing, tst)
	| otherwise
		# (users, tst)		= userStore id tst
		= case [u \\ u <- users | u.userName == username && u.password == (Password password)] of
			[user]	= (Just user, tst)		
			_		= (Nothing, tst)

createUser :: !User !*TSt -> (!User,!*TSt)
createUser user tst
	# (users, tst)		= userStore id tst
	# (users, tst)		= userStore (\_-> [user:users]) tst
	= (user,tst)
	
updateUser :: !User !*TSt -> (!User,!*TSt)
updateUser user tst
	# (users,tst)		= userStore (map (update user)) tst
	= (user,tst)
where
	update new old	= if (old.User.userName == new.User.userName) new old

deleteUser :: !User !*TSt -> (!User,!*TSt)
deleteUser user tst
	# (users,tst)		= userStore delete tst
	= (user,tst)
where
	delete users	= [u \\ u <- users | u.User.userName <> user.User.userName]
	
tidyUserName :: !UserName !*TSt -> (!UserName, !*TSt)
tidyUserName (UserName uid disp) tst
	# (user,tst) = getUser (UserName uid disp) tst
	| user == unknownUser 	= (UserName uid "unregistered",tst)
	| otherwise 			= (UserName uid user.displayName,tst)
	
//Helper function which finds a property of a certain user
lookupUserProperty :: ![User] !(User -> a) !a !UserName -> a
lookupUserProperty users selectFunction defaultValue userName
		= case [selectFunction user \\ user <- users | toUserName user == userName] of
			[x] = x
			_	= defaultValue

userStore ::  !([User] -> [User]) !*TSt -> (![User],!*TSt) 	
userStore fn tst=:{TSt|staticInfo,world}
	# (users,world)			= readUserFile staticInfo.appName world 
	# users					= fn users
	# world					= writeUserFile users staticInfo.appName world
	= (users,{TSt|tst & world = world})
where
	readUserFile appName world
		# (content,world) = readfile (appName +++ "-users.json") world
		| content == ""
			= ([],world)
		| otherwise
			= case (fromJSON content) of
				Just users	= (users,world)
				Nothing		= ([],world)
				
	writeUserFile users appName world
		= writefile (appName +++ "-users.json") (toJSON users) world
		
			
