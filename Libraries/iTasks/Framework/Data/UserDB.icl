implementation module UserDB

import StdEnv, StdMaybe
import StdGeneric, GenBimap
import TSt, Store

derive gPrint User
derive gParse User
		  
initUsers :: [User] 
initUsers	= [ {User | userId = 0, username = "root", displayname = "Root", password = "", roles = ["root", "president","manager","worker"]}
			  , {User | userId = 1, username = "president", displayname = "President", password = "", roles = ["president"]}
			  , {User | userId = 2, username = "manager", displayname = "Middle manager", password = "", roles = ["manager"]}
			  , {User | userId = 3, username = "worker1", displayname = "Office worker 1", password = "", roles = ["worker"]}
			  
			  , {User | userId = 10, username = "customer", displayname = "Customer", password = "", roles = ["customer"]}
			  , {User | userId = 11, username = "bank", displayname = "Bank authorization", password = "", roles = ["bank"]}
			  , {User | userId = 12, username = "storage", displayname = "Webshop storage", password = "", roles = ["storage"]}
			  , {User | userId = 13, username = "creditcard", displayname = "Creditcard authorization", password = "", roles = ["creditcard"]}    
			  
			  , {User | userId = 20, username = "megastore", displayname = "Megastore", password = "", roles = ["supplier"]}
			  , {User | userId = 21, username = "localshop", displayname = "Local shop", password = "", roles = ["supplier"]}
			  , {User | userId = 22, username = "webshop", displayname = "Webshop.com", password = "", roles = ["supplier"]}   

			  , {User | userId = 30, username = "ambupost0", displayname = "Ambulance Post 0", password = "", roles = ["ambulances"]}
			  , {User | userId = 31, username = "ambupost1", displayname = "Ambulance Post 1", password = "", roles = ["ambulances"]}
			  , {User | userId = 32, username = "ambupost2", displayname = "Ambulance Post 2", password = "", roles = ["ambulances"]}   
			  , {User | userId = 33, username = "ambupost3", displayname = "Ambulance Post 3", password = "", roles = ["ambulances"]}   
			  , {User | userId = 34, username = "ambupost4", displayname = "Ambulance Post 4", password = "", roles = ["ambulances"]}   
			  , {User | userId = 35, username = "ambupost5", displayname = "Ambulance Post 5", password = "", roles = ["ambulances"]}   
			  , {User | userId = 36, username = "ambupost6", displayname = "Ambulance Post 6", password = "", roles = ["ambulances"]}   
			  , {User | userId = 37, username = "ambupost7", displayname = "Ambulance Post 7", password = "", roles = ["ambulances"]}   
			  , {User | userId = 38, username = "ambupost8", displayname = "Ambulance Post 8", password = "", roles = ["ambulances"]}   
			  , {User | userId = 39, username = "ambupost9", displayname = "Ambulance Post 9", password = "", roles = ["ambulances"]}   

			  , {User | userId = 40, username = "expert0", displayname = "Expert 0", password = "", roles = ["experts"]}
			  , {User | userId = 41, username = "expert1", displayname = "Expert 1", password = "", roles = ["experts"]}
			  , {User | userId = 42, username = "expert2", displayname = "Expert 2", password = "", roles = ["experts"]}   
			  , {User | userId = 43, username = "expert3", displayname = "Expert 3", password = "", roles = ["experts"]}   
			  , {User | userId = 44, username = "expert4", displayname = "Expert 4", password = "", roles = ["experts"]}   
			  , {User | userId = 45, username = "expert5", displayname = "Expert 5", password = "", roles = ["experts"]}   
			  , {User | userId = 46, username = "expert6", displayname = "Expert 6", password = "", roles = ["experts"]}   
			  , {User | userId = 47, username = "expert7", displayname = "Expert 7", password = "", roles = ["experts"]}   
			  , {User | userId = 48, username = "expert8", displayname = "Expert 8", password = "", roles = ["experts"]}   
			  , {User | userId = 49, username = "expert9", displayname = "Expert 9", password = "", roles = ["experts"]}   
			  ]
			  
instance UserDB TSt
where
	getUser :: !Int !*TSt -> (!(Int,String),!*TSt)
	getUser uid tst
		# (users, tst)		= userStore id tst
		= case [(user.User.userId, user.User.displayname) \\ user <- users | user.User.userId == uid] of
			[x] = (x,tst)
			_	= ((-1, "Unknown user"),tst)

	getUsers :: !*TSt -> (![(Int,String)], !*TSt)
	getUsers tst
		# (users, tst)		= userStore id tst
		= ([(user.User.userId, user.User.displayname) \\ user <- users], tst)

	getUsersWithRole :: !String !*TSt -> (![(Int,String)], !*TSt)
	getUsersWithRole role tst
		# (users, tst)		= userStore id tst
		= ([(user.User.userId,user.User.displayname) \\ user <- users | isMember role user.User.roles], tst)		

	getDisplayNames	:: ![Int] !*TSt -> (![String], !*TSt)
	getDisplayNames	uids tst
		# (users, tst)		= userStore id tst
		= (map (lookupUserProperty users (\u -> u.displayname) "Unknown user") uids, tst)
	
	getUserNames :: ![Int] !*TSt -> (![String], !*TSt)
	getUserNames uids tst
		# (users, tst)		= userStore id tst
		= (map (lookupUserProperty users (\u -> u.username) "") uids, tst)
	
	getRoles :: ![Int] !*TSt -> (![[String]], !*TSt)
	getRoles uids tst
		# (users, tst)		= userStore id tst
		= (map (lookupUserProperty users (\u -> u.User.roles) []) uids, tst)

	authenticateUser :: !String !String	!*TSt -> (!Maybe (Int,String,[String]), !*TSt)
	authenticateUser username password tst
		# (users, tst)		= userStore id tst
		= case [u \\ u <- users | u.username == username && u.password == password] of
			[user]	= (Just (user.User.userId, user.User.displayname, user.User.roles), tst)		
			_		= (Nothing, tst)

//Helper function which finds a property of a certain user
lookupUserProperty :: ![User] !(User -> a) !a !Int -> a
lookupUserProperty users selectFunction defaultValue userId
		= case [selectFunction user \\ user <- users | user.User.userId == userId] of
			[x] = x
			_	= defaultValue

userStore ::  !([User] -> [User]) !*TSt -> (![User],!*TSt) 	
userStore fn tst=:{store,world}
	# (mbList,store,world)	= loadValue "UserDB" store world
	# list 					= fn (case mbList of Nothing = initUsers; Just list = list)
	# store					= storeValue "UserDB" list store 
	= (list, {tst & store = store, world = world})