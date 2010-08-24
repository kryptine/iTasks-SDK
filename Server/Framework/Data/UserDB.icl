implementation module UserDB

import StdEnv, StdMaybe
import StdGeneric
import TSt, Util

from Types import :: Password(..)

derive bimap (,), Maybe

instance UserDB IWorld
where
	getUser :: !UserId !*IWorld -> (!Maybe User,!*IWorld)
	getUser "root" iworld
		= (Just RootUser,iworld)
	getUser userName iworld
		# (users, iworld) = userStore id iworld
		= case filter ((==) (NamedUser userName)) users of
			[x] = (Just x,iworld)
			_	= (Nothing,iworld)
			
	getUsers :: !*IWorld -> (![User], !*IWorld)
	getUsers iworld
		# (users, iworld) = userStore id iworld
		= (users,iworld)	//Do not include the "root" user"
		
	getUsersWithRole :: !String !*IWorld -> (![User], !*IWorld)
	getUsersWithRole role iworld
		# (users, iworld)		= userStore id iworld
		= ([u \\ u=:(RegisteredUser d) <- users | isMember role (mb2list d.UserDetails.roles)], iworld)
		
	authenticateUser :: !String !String	!*IWorld -> (!Maybe User, !*IWorld)
	authenticateUser username password iworld
		| username == "root"
			| password	== iworld.config.rootPassword
				= (Just RootUser, iworld)
			| otherwise
				= (Nothing, iworld)
		| otherwise
			# (users, iworld)		= userStore id iworld
			= case [u \\ u=:(RegisteredUser d) <- users | d.userName == username && d.password == (Password password)] of
				[user]	= (Just user, iworld)		
				_		= (Nothing, iworld)
	
	createUser :: !UserDetails !*IWorld -> (!User,!*IWorld)
	createUser details iworld
		# (users, iworld)		= userStore id iworld
		# (users, iworld)		= userStore (\_-> [RegisteredUser details:users]) iworld
		= (RegisteredUser details,iworld)
		
	updateUser :: !User !UserDetails !*IWorld -> (!User,!*IWorld)
	updateUser match details iworld
		# (users,iworld)		= userStore (map (update match new)) iworld
		= (new,iworld)
	where
		new = RegisteredUser details
		update match details old	= if (old == match) new old
	
	deleteUser :: !User !*IWorld -> (!User,!*IWorld)
	deleteUser user iworld
		# (users,iworld)		= userStore delete iworld
		= (user,iworld)
	where
		delete users	= removeMember user users
	
//Helper function which finds a property of a certain user
lookupUserProperty :: ![User] !(User -> a) !a !UserId -> a
lookupUserProperty users selectFunction defaultValue userName
		= case [selectFunction user \\ user=:(RegisteredUser d) <- users | d.UserDetails.userName == userName] of
			[x] = x
			_	= defaultValue

userStore ::  !([User] -> [User]) !*IWorld -> (![User],!*IWorld) 	
userStore fn iworld=:{IWorld|application,world}
	# (users,world)			= readUserFile application world 
	# users					= fn users
	# world					= writeUserFile users application world
	= (users,{IWorld|iworld & world = world})
where
	readUserFile appName world
		# (content,world) = readfile (appName +++ "-users.json") world
		# world = world
		| content == ""
			= ([],world)
		| otherwise
			= case (fromJSON (fromString content)) of
				Just users	= (users,world)
				Nothing		= ([],world)
				
	writeUserFile users appName world
		= writefile (appName +++ "-users.json") (toString (toJSON users)) world
		
instance UserDB TSt
where
	getUser :: !UserId !*TSt -> (!Maybe User,!*TSt)	
	getUser userId tst = accIWorldTSt (getUser userId) tst
	getUsers :: !*TSt -> (![User], !*TSt)
	getUsers tst = accIWorldTSt getUsers tst
	getUsersWithRole :: !String !*TSt -> (![User], !*TSt)
	getUsersWithRole role tst = accIWorldTSt (getUsersWithRole role) tst
	authenticateUser :: !String !String	!*TSt -> (!Maybe User, !*TSt)
	authenticateUser username password tst = accIWorldTSt (authenticateUser username password) tst
	createUser :: !UserDetails !*TSt -> (!User,!*TSt)
	createUser details tst = accIWorldTSt (createUser details) tst
	updateUser :: !User !UserDetails !*TSt -> (!User,!*TSt)
	updateUser user details tst = accIWorldTSt (updateUser user details) tst
	deleteUser :: !User !*TSt -> (!User,!*TSt)
	deleteUser user tst = accIWorldTSt (deleteUser user) tst