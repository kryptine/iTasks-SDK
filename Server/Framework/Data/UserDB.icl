implementation module UserDB

import StdEnv, Maybe
import StdGeneric
import File, Error
import TSt, Util

from Types import :: Password(..)

derive bimap (,), Maybe

instance UserDB IWorld
where
	getUser :: !UserId !*IWorld -> (!Maybe User,!*IWorld)
	getUser "root" iworld
		= (Just RootUser,iworld)
	getUser userName iworld
		# (details, iworld) = readUserStore iworld
		= case filter (\d -> (==) (NamedUser userName) (RegisteredUser d)) details of
			[x] = (Just (RegisteredUser x),iworld)
			_	= (Nothing,iworld)
	
	getUserDetails :: !User !*IWorld -> (!Maybe UserDetails,!*IWorld)
	getUserDetails RootUser iworld=:{IWorld|config}
		= (Just {UserDetails
			|userName = "root"
			,password = Password config.rootPassword
			,displayName = "Root User"
			,emailAddress = EmailAddress config.rootEmail
			,roles = Nothing},iworld)
			
	getUserDetails (RegisteredUser details) iworld = (Just details,iworld)
	getUserDetails (NamedUser username) iworld
		# (details, iworld) = readUserStore iworld
		= case filter (\d -> (==) (NamedUser username) (RegisteredUser d)) details of
			[x] = (Just x,iworld)
			_	= (Nothing,iworld)
	getUserDetails _ iworld = (Nothing,iworld)
			
	getUsers :: !*IWorld -> (![User], !*IWorld)
	getUsers iworld
		# (details, iworld) = readUserStore iworld
		= (map (\d -> RegisteredUser d) details,iworld)	//Do not include the "root" user"
		
	getUsersWithRole :: !String !*IWorld -> (![User], !*IWorld)
	getUsersWithRole role iworld
		# (details, iworld)		= readUserStore iworld
		= ([(RegisteredUser d) \\ d <- details | isMember role (mb2list d.UserDetails.roles)], iworld)
		
	authenticateUser :: !String !String	!*IWorld -> (!Maybe User, !*IWorld)
	authenticateUser username password iworld
		| username == "root"
			| password	== iworld.config.rootPassword
				= (Just RootUser, iworld)
			| otherwise
				= (Nothing, iworld)
		| otherwise
			# (details, iworld)	= readUserStore iworld
			= case [(RegisteredUser d) \\ d <- details | d.userName == username && d.password == (Password password)] of
				[user]	= (Just user, iworld)		
				_		= (Nothing, iworld)
	
	createUser :: !UserDetails !*IWorld -> (!User,!*IWorld)
	createUser details iworld
		# (store, iworld)		= readUserStore iworld
		# (store, iworld)		= userStore (\_-> [details:store]) iworld
		= (RegisteredUser details,iworld)
		
	updateUser :: !User !UserDetails !*IWorld -> (!User,!*IWorld)
	updateUser match details iworld
		# (store,iworld)		= userStore (map (update match details)) iworld
		= (RegisteredUser details,iworld)
	where
		update match details old	= if (RegisteredUser old == match) details old
	
	deleteUser :: !User !*IWorld -> (!User,!*IWorld)
	deleteUser user iworld
		# (store,iworld)		= userStore delete iworld
		= (user,iworld)
	where
		delete details	= filter (\d -> (RegisteredUser d) <> user) details
		
	lastChange :: !*IWorld -> (!Timestamp,!*IWorld)
	lastChange iworld =:{IWorld|application,world}
		# ((ts,_),world) = readUserFile application world 
		= (ts,{iworld & world = world})
	
//Helper function which finds a property of a certain user
lookupUserProperty :: ![User] !(User -> a) !a !UserId -> a
lookupUserProperty users selectFunction defaultValue userName
		= case [selectFunction user \\ user=:(RegisteredUser d) <- users | d.UserDetails.userName == userName] of
			[x] = x
			_	= defaultValue

userStore :: !([UserDetails] -> [UserDetails]) !*IWorld -> (![UserDetails],!*IWorld) 	
userStore fn iworld=:{IWorld|application,world,timestamp}
	# ((_,users),world)		= readUserFile application world 
	# users					= fn users
	# world					= writeUserFile (timestamp,users) application world
	= (users,{IWorld|iworld & world = world})
where
	writeUserFile users appName world
		# (_, world) = writeFile (appName +++ USER_FILE_POSTFIX) (toString (toJSON users)) world
		= world
		
readUserStore :: !*IWorld -> (![UserDetails],!*IWorld) 	
readUserStore iworld=:{IWorld|application,world}
	# ((Timestamp _,users),world) = readUserFile application world 
	= (users,{IWorld|iworld & world = world})

readUserFile :: !String !*World -> (!(!Timestamp,![UserDetails]),!*World)	
readUserFile appName world
	# (res,world) = readFile (appName +++ USER_FILE_POSTFIX) world
	| isError res = ((Timestamp 0,[]),world)
	= case (fromJSON (fromString (fromOk res))) of
			Just users	= (users,world)
			Nothing		= ((Timestamp 0,[]),world)
		
USER_FILE_POSTFIX :== "-users.json"
		
instance UserDB TSt
where
	getUser :: !UserId !*TSt -> (!Maybe User,!*TSt)	
	getUser userId tst = accIWorldTSt (getUser userId) tst
	getUserDetails :: !User !*TSt -> (!Maybe UserDetails,!*TSt)
	getUserDetails user tst = accIWorldTSt (getUserDetails user) tst
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
	lastChange :: !*TSt -> (!Timestamp,!*TSt)
	lastChange tst = accIWorldTSt lastChange tst