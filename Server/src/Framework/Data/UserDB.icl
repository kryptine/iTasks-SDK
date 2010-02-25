implementation module UserDB

import StdEnv, StdMaybe
import StdGeneric
import TSt, Store

unknownUser :: User
unknownUser = {User | userName = "", displayName = "Unknown user", password = "", roles = []}

rootUser :: User
rootUser = {User | userName = "root", displayName = "Root", password = "", roles = []}

//TEMPORARY ALTERNATIVE HARDCODED USER SET
testUsers :: [User]
testUsers = [ {User | userName = "bas", displayName = "Bas", password = "", roles = []}
			, {User | userName = "rinus", displayName = "Rinus", password = "", roles = []}
			, {User | userName = "thomas", displayName = "Thomas", password = "", roles = []}
			, {User | userName = "peter", displayName = "Peter", password = "", roles = []}
			, {User | userName = "pieter", displayName = "Pieter", password = "", roles = []}
			, {User | userName = "janmartin", displayName = "Jan Martin", password = "", roles = []}
			, {User | userName = "steffen", displayName = "Steffen Michels", password = "", roles = []}

  			, {User | userName = "megastore", displayName = "Megastore", password = "", roles = ["supplier"]}
			, {User | userName = "localshop", displayName = "Local shop", password = "", roles = ["supplier"]}
			, {User | userName = "webshop", displayName = "Webshop.com", password = "", roles = ["supplier"]}   
			
			, {User | userName = "ambupost0", displayName = "Ambulance Post 0", password = "", roles = ["ambulances"]}
			, {User | userName = "ambupost1", displayName = "Ambulance Post 1", password = "", roles = ["ambulances"]}
			, {User | userName = "ambupost2", displayName = "Ambulance Post 2", password = "", roles = ["ambulances"]}   
			, {User | userName = "ambupost3", displayName = "Ambulance Post 3", password = "", roles = ["ambulances"]}   
			, {User | userName = "ambupost4", displayName = "Ambulance Post 4", password = "", roles = ["ambulances"]}   
			, {User | userName = "ambupost5", displayName = "Ambulance Post 5", password = "", roles = ["ambulances"]}   
			, {User | userName = "ambupost6", displayName = "Ambulance Post 6", password = "", roles = ["ambulances"]}   
			, {User | userName = "ambupost7", displayName = "Ambulance Post 7", password = "", roles = ["ambulances"]}   
			, {User | userName = "ambupost8", displayName = "Ambulance Post 8", password = "", roles = ["ambulances"]}   
			, {User | userName = "ambupost9", displayName = "Ambulance Post 9", password = "", roles = ["ambulances"]}  
			
			]
/*
testUsers :: [User] 
testUsers	= [ {User | userName = "president", displayName = "President", password = "", roles = ["president"]}
			  , {User | userName = "manager", displayName = "Middle manager", password = "", roles = ["manager"]}
			  , {User | userName = "worker1", displayName = "Office worker 1", password = "", roles = ["worker"]}
			  
			  , {User | userName = "customer", displayName = "Customer", password = "", roles = ["customer"]}
			  , {User | userName = "bank", displayName = "Bank authorization", password = "", roles = ["bank"]}
			  , {User | userName = "storage", displayName = "Webshop storage", password = "", roles = ["storage"]}
			  , {User | userName = "creditcard", displayName = "Creditcard authorization", password = "", roles = ["creditcard"]}    
			  
			  , {User | userName = "megastore", displayName = "Megastore", password = "", roles = ["supplier"]}
			  , {User | userName = "localshop", displayName = "Local shop", password = "", roles = ["supplier"]}
			  , {User | userName = "webshop", displayName = "Webshop.com", password = "", roles = ["supplier"]}   

			  , {User | userName = "ambupost0", displayName = "Ambulance Post 0", password = "", roles = ["ambulances"]}
			  , {User | userName = "ambupost1", displayName = "Ambulance Post 1", password = "", roles = ["ambulances"]}
			  , {User | userName = "ambupost2", displayName = "Ambulance Post 2", password = "", roles = ["ambulances"]}   
			  , {User | userName = "ambupost3", displayName = "Ambulance Post 3", password = "", roles = ["ambulances"]}   
			  , {User | userName = "ambupost4", displayName = "Ambulance Post 4", password = "", roles = ["ambulances"]}   
			  , {User | userName = "ambupost5", displayName = "Ambulance Post 5", password = "", roles = ["ambulances"]}   
			  , {User | userName = "ambupost6", displayName = "Ambulance Post 6", password = "", roles = ["ambulances"]}   
			  , {User | userName = "ambupost7", displayName = "Ambulance Post 7", password = "", roles = ["ambulances"]}   
			  , {User | userName = "ambupost8", displayName = "Ambulance Post 8", password = "", roles = ["ambulances"]}   
			  , {User | userName = "ambupost9", displayName = "Ambulance Post 9", password = "", roles = ["ambulances"]}   

			  , {User | userName = "expert0", displayName = "Expert 0", password = "", roles = ["experts"]}
			  , {User | userName = "expert1", displayName = "Expert 1", password = "", roles = ["experts"]}
			  , {User | userName = "expert2", displayName = "Expert 2", password = "", roles = ["experts"]}   
			  , {User | userName = "expert3", displayName = "Expert 3", password = "", roles = ["experts"]}   
			  , {User | userName = "expert4", displayName = "Expert 4", password = "", roles = ["experts"]}   
			  , {User | userName = "expert5", displayName = "Expert 5", password = "", roles = ["experts"]}   
			  , {User | userName = "expert6", displayName = "Expert 6", password = "", roles = ["experts"]}   
			  , {User | userName = "expert7", displayName = "Expert 7", password = "", roles = ["experts"]}   
			  , {User | userName = "expert8", displayName = "Expert 8", password = "", roles = ["experts"]}   
			  , {User | userName = "expert9", displayName = "Expert 9", password = "", roles = ["experts"]}   
			  ]	
*/

getUser :: !UserName !*TSt -> (!User,!*TSt)
getUser "root" tst
	= (rootUser,tst)
getUser userName tst
	# (users, tst)		= userStore id tst
	= case filter (\u -> u.User.userName == userName) users of
		[x] = (x,tst)
		_	= (unknownUser,tst)

getUserByName :: !String !*TSt -> (!User, !*TSt)
getUserByName "root" tst
	= (rootUser,tst)
getUserByName name tst
	# (users, tst)		= userStore id tst
	= case filter (\u -> u.User.userName == name) users of
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
	displayName users "root" = "Root"
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
		= case [u \\ u <- users | u.userName == username && u.password == password] of
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
	
//Helper function which finds a property of a certain user
lookupUserProperty :: ![User] !(User -> a) !a !UserName -> a
lookupUserProperty users selectFunction defaultValue userName
		= case [selectFunction user \\ user <- users | user.User.userName == userName] of
			[x] = x
			_	= defaultValue

userStore ::  !([User] -> [User]) !*TSt -> (![User],!*TSt) 	
userStore fn tst=:{TSt|systemStore,world}
	# (mbList,sstore,world)	= loadValue "UserDB" systemStore world
	# list 					= fn (case mbList of Nothing = testUsers; Just list = list)
	# sstore				= storeValue "UserDB" list sstore 
	= (list, {TSt|tst & systemStore = sstore, world = world})