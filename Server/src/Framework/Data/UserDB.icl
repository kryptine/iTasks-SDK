implementation module UserDB

import StdEnv, StdMaybe
import StdGeneric
import TSt, Util

from Types import :: Password(..)

derive JSONEncode User, UserDetails, Password
derive JSONDecode User, UserDetails, Password
derive bimap (,), Maybe

getUser :: !UserId !*TSt -> (!User,!*TSt)
getUser "root" tst
	= (RootUser,tst)
getUser userName tst
	# (users, tst) = userStore id tst
	= case filter ((==) (NamedUser userName)) users of
		[x] = (x,tst)
		_	= (AnyUser,tst)

getUserByName :: !String !*TSt -> (!User, !*TSt)
getUserByName "root" tst
	= (RootUser,tst)
getUserByName userName tst
	# (users, tst)		= userStore id tst
	= case [u \\ u=:(RegisteredUser d) <- users | d.UserDetails.userName == userName ] of
		[x] = (x,tst)
		_	= (AnyUser,tst)
	
getUsers :: !*TSt -> (![User], !*TSt)
getUsers tst
	# (users, tst) = userStore id tst
	= (users,tst)	//Do not include the "root" user"
	
getUsersWithRole :: !String !*TSt -> (![User], !*TSt)
getUsersWithRole role tst
	# (users, tst)		= userStore id tst
	= ([u \\ u=:(RegisteredUser d) <- users | isMember role d.UserDetails.roles], tst)
	
authenticateUser :: !String !String	!*TSt -> (!Maybe User, !*TSt)
authenticateUser username password tst
	| username == "root"
		| password	== tst.config.rootPassword
			= (Just RootUser, tst)
		| otherwise
			= (Nothing, tst)
	| otherwise
		# (users, tst)		= userStore id tst
		= case [u \\ u=:(RegisteredUser d) <- users | d.userName == username && d.password == (Password password)] of
			[user]	= (Just user, tst)		
			_		= (Nothing, tst)

createUser :: !UserDetails !*TSt -> (!User,!*TSt)
createUser details tst
	# (users, tst)		= userStore id tst
	# (users, tst)		= userStore (\_-> [RegisteredUser details:users]) tst
	= (RegisteredUser details,tst)
	
updateUser :: !User !UserDetails !*TSt -> (!User,!*TSt)
updateUser match details tst
	# (users,tst)		= userStore (map (update match new)) tst
	= (new,tst)
where
	new = RegisteredUser details
	update match details old	= if (old == match) new old

deleteUser :: !User !*TSt -> (!User,!*TSt)
deleteUser user tst
	# (users,tst)		= userStore delete tst
	= (user,tst)
where
	delete users	= removeMember user users
	
//Helper function which finds a property of a certain user
lookupUserProperty :: ![User] !(User -> a) !a !UserId -> a
lookupUserProperty users selectFunction defaultValue userName
		= case [selectFunction user \\ user=:(RegisteredUser d) <- users | d.UserDetails.userName == userName] of
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
			= case (fromJSON (fromString content)) of
				Just users	= (users,world)
				Nothing		= ([],world)
				
	writeUserFile users appName world
		= writefile (appName +++ "-users.json") (toString (toJSON users)) world
		
			
