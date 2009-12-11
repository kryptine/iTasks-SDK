implementation module UserDB

import StdEnv, StdMaybe
import StdGeneric, GenBimap
import TSt, Store

unknownUser :: User
unknownUser = {User | userId = -1, userName = "", displayName = "Unknown user", password = "", roles = []}

rootUser :: User
rootUser = {User | userId = 0, userName = "root", displayName = "Root", password = "", roles = []}

//TEMPORARY ALTERNATIVE HARDCODED USER SET
testUsers :: [User]
testUsers = [ {User | userId = 1, userName = "bas", displayName = "Bas", password = "", roles = []}
			, {User | userId = 2, userName = "rinus", displayName = "Rinus", password = "", roles = []}
			, {User | userId = 3, userName = "thomas", displayName = "Thomas", password = "", roles = []}
			, {User | userId = 4, userName = "peter", displayName = "Peter", password = "", roles = []}
			, {User | userId = 5, userName = "pieter", displayName = "Pieter", password = "", roles = []}
			, {User | userId = 6, userName = "janmartin", displayName = "Jan Martin", password = "", roles = []}

  			, {User | userId = 20, userName = "megastore", displayName = "Megastore", password = "", roles = ["supplier"]}
			, {User | userId = 21, userName = "localshop", displayName = "Local shop", password = "", roles = ["supplier"]}
			, {User | userId = 22, userName = "webshop", displayName = "Webshop.com", password = "", roles = ["supplier"]}   
			
			, {User | userId = 30, userName = "ambupost0", displayName = "Ambulance Post 0", password = "", roles = ["ambulances"]}
			, {User | userId = 31, userName = "ambupost1", displayName = "Ambulance Post 1", password = "", roles = ["ambulances"]}
			, {User | userId = 32, userName = "ambupost2", displayName = "Ambulance Post 2", password = "", roles = ["ambulances"]}   
			, {User | userId = 33, userName = "ambupost3", displayName = "Ambulance Post 3", password = "", roles = ["ambulances"]}   
			, {User | userId = 34, userName = "ambupost4", displayName = "Ambulance Post 4", password = "", roles = ["ambulances"]}   
			, {User | userId = 35, userName = "ambupost5", displayName = "Ambulance Post 5", password = "", roles = ["ambulances"]}   
			, {User | userId = 36, userName = "ambupost6", displayName = "Ambulance Post 6", password = "", roles = ["ambulances"]}   
			, {User | userId = 37, userName = "ambupost7", displayName = "Ambulance Post 7", password = "", roles = ["ambulances"]}   
			, {User | userId = 38, userName = "ambupost8", displayName = "Ambulance Post 8", password = "", roles = ["ambulances"]}   
			, {User | userId = 39, userName = "ambupost9", displayName = "Ambulance Post 9", password = "", roles = ["ambulances"]}  
			
			]
/*
testUsers :: [User] 
testUsers	= [ {User | userId = 1, userName = "president", displayName = "President", password = "", roles = ["president"]}
			  , {User | userId = 2, userName = "manager", displayName = "Middle manager", password = "", roles = ["manager"]}
			  , {User | userId = 3, userName = "worker1", displayName = "Office worker 1", password = "", roles = ["worker"]}
			  
			  , {User | userId = 10, userName = "customer", displayName = "Customer", password = "", roles = ["customer"]}
			  , {User | userId = 11, userName = "bank", displayName = "Bank authorization", password = "", roles = ["bank"]}
			  , {User | userId = 12, userName = "storage", displayName = "Webshop storage", password = "", roles = ["storage"]}
			  , {User | userId = 13, userName = "creditcard", displayName = "Creditcard authorization", password = "", roles = ["creditcard"]}    
			  
			  , {User | userId = 20, userName = "megastore", displayName = "Megastore", password = "", roles = ["supplier"]}
			  , {User | userId = 21, userName = "localshop", displayName = "Local shop", password = "", roles = ["supplier"]}
			  , {User | userId = 22, userName = "webshop", displayName = "Webshop.com", password = "", roles = ["supplier"]}   

			  , {User | userId = 30, userName = "ambupost0", displayName = "Ambulance Post 0", password = "", roles = ["ambulances"]}
			  , {User | userId = 31, userName = "ambupost1", displayName = "Ambulance Post 1", password = "", roles = ["ambulances"]}
			  , {User | userId = 32, userName = "ambupost2", displayName = "Ambulance Post 2", password = "", roles = ["ambulances"]}   
			  , {User | userId = 33, userName = "ambupost3", displayName = "Ambulance Post 3", password = "", roles = ["ambulances"]}   
			  , {User | userId = 34, userName = "ambupost4", displayName = "Ambulance Post 4", password = "", roles = ["ambulances"]}   
			  , {User | userId = 35, userName = "ambupost5", displayName = "Ambulance Post 5", password = "", roles = ["ambulances"]}   
			  , {User | userId = 36, userName = "ambupost6", displayName = "Ambulance Post 6", password = "", roles = ["ambulances"]}   
			  , {User | userId = 37, userName = "ambupost7", displayName = "Ambulance Post 7", password = "", roles = ["ambulances"]}   
			  , {User | userId = 38, userName = "ambupost8", displayName = "Ambulance Post 8", password = "", roles = ["ambulances"]}   
			  , {User | userId = 39, userName = "ambupost9", displayName = "Ambulance Post 9", password = "", roles = ["ambulances"]}   

			  , {User | userId = 40, userName = "expert0", displayName = "Expert 0", password = "", roles = ["experts"]}
			  , {User | userId = 41, userName = "expert1", displayName = "Expert 1", password = "", roles = ["experts"]}
			  , {User | userId = 42, userName = "expert2", displayName = "Expert 2", password = "", roles = ["experts"]}   
			  , {User | userId = 43, userName = "expert3", displayName = "Expert 3", password = "", roles = ["experts"]}   
			  , {User | userId = 44, userName = "expert4", displayName = "Expert 4", password = "", roles = ["experts"]}   
			  , {User | userId = 45, userName = "expert5", displayName = "Expert 5", password = "", roles = ["experts"]}   
			  , {User | userId = 46, userName = "expert6", displayName = "Expert 6", password = "", roles = ["experts"]}   
			  , {User | userId = 47, userName = "expert7", displayName = "Expert 7", password = "", roles = ["experts"]}   
			  , {User | userId = 48, userName = "expert8", displayName = "Expert 8", password = "", roles = ["experts"]}   
			  , {User | userId = 49, userName = "expert9", displayName = "Expert 9", password = "", roles = ["experts"]}   
			  ]	
*/

getUser :: !Int !*TSt -> (!User,!*TSt)
getUser 0 tst
	= (rootUser,tst)
getUser uid tst
	# (users, tst)		= userStore id tst
	= case filter (\u -> u.User.userId == uid) users of
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

getDisplayNames	:: ![Int] !*TSt -> (![String], !*TSt)
getDisplayNames	uids tst
	# (users, tst)		= userStore id tst
	= (map (displayName users) uids, tst)
where
	displayName users 0 = "Root"
	displayName users uid = lookupUserProperty users (\u -> u.displayName) "Unknown user" uid
	
getUserNames :: ![Int] !*TSt -> (![String], !*TSt)
getUserNames uids tst
	# (users, tst)		= userStore id tst
	= (map (userName users) uids, tst)
where
	userName users 0 = "root"
	userName users uid = lookupUserProperty users (\u -> u.userName) "" uid
	
getRoles :: ![Int] !*TSt -> (![[String]], !*TSt)
getRoles uids tst
	# (users, tst)		= userStore id tst
	= (map (lookupUserProperty users (\u -> u.User.roles) []) uids, tst)

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


createUser :: !String !String !String ![String] !*TSt -> (User,!*TSt)
createUser username password displayname roles tst
	# (users, tst)		= userStore id tst
	# user	= {userId= maxid users, userName = username, password = password, displayName = displayname, roles = roles}
	# (users, tst)		= userStore (\_-> [user:users]) tst
	= (user,tst)
where
	maxid [] = 1 
	maxid users	= maxList [user.User.userId \\ user <- users] + 1

//Helper function which finds a property of a certain user
lookupUserProperty :: ![User] !(User -> a) !a !Int -> a
lookupUserProperty users selectFunction defaultValue userId
		= case [selectFunction user \\ user <- users | user.User.userId == userId] of
			[x] = x
			_	= defaultValue

userStore ::  !([User] -> [User]) !*TSt -> (![User],!*TSt) 	
userStore fn tst=:{TSt|systemStore,world}
	# (mbList,sstore,world)	= loadValue "UserDB" systemStore world
	# list 					= fn (case mbList of Nothing = testUsers; Just list = list)
	# sstore				= storeValue "UserDB" list sstore 
	= (list, {TSt|tst & systemStore = sstore, world = world})