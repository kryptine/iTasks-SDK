implementation module UserDB

import StdEnv, StdMaybe
import StdGeneric, GenBimap
import HSt, TSt, iDataFormlib

derive gForm User
derive gUpd User
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
			  
instance UserDB HSt
where
	getUser :: !Int !*HSt -> (!(Int,String),!*HSt)
	getUser uid hst
		# (users, hst)		= userStore id hst
		= case [(user.User.userId, user.User.displayname) \\ user <- users | user.User.userId == uid] of
			[x] = (x,hst)
			_	= ((-1, "Unknown user"),hst)

	getUsers :: !*HSt -> (![(Int,String)], !*HSt)
	getUsers hst
		# (users, hst)		= userStore id hst
		= ([(user.User.userId, user.User.displayname) \\ user <- users], hst)

	getUsersWithRole :: !String !*HSt -> (![(Int,String)], !*HSt)
	getUsersWithRole role hst
		# (users, hst)		= userStore id hst
		= ([(user.User.userId,user.User.displayname) \\ user <- users | isMember role user.User.roles], hst)		

	getDisplayNames	:: ![Int] !*HSt -> (![String], !*HSt)
	getDisplayNames	uids hst
		# (users, hst)		= userStore id hst
		= (map (lookupUserProperty users (\u -> u.displayname) "Unknown user") uids, hst)
	
	getUserNames :: ![Int] !*HSt -> (![String], !*HSt)
	getUserNames uids hst
		# (users, hst)		= userStore id hst
		= (map (lookupUserProperty users (\u -> u.username) "") uids, hst)
	
	getRoles :: ![Int] !*HSt -> (![[String]], !*HSt)
	getRoles uids hst
		# (users, hst)		= userStore id hst
		= (map (lookupUserProperty users (\u -> u.User.roles) []) uids, hst)

	authenticateUser :: !String !String	!*HSt -> (!Maybe (Int,String,[String]), !*HSt)
	authenticateUser username password hst
		# (users, hst)		= userStore id hst
		= case [u \\ u <- users | u.username == username && u.password == password] of
			[user]	= (Just (user.User.userId, user.User.displayname, user.User.roles), hst)		
			_		= (Nothing, hst)

//Helper function which finds a property of a certain user
lookupUserProperty :: ![User] !(User -> a) !a !Int -> a
lookupUserProperty users selectFunction defaultValue userId
		= case [selectFunction user \\ user <- users | user.User.userId == userId] of
			[x] = x
			_	= defaultValue

userStore ::  !([User] -> [User]) !*HSt -> (![User],!*HSt) 
userStore fn hst		
	# (form,hst) = mkStoreForm (Init, pFormId "UserDB" initUsers) fn hst
	= (form.Form.value, hst)

instance UserDB TSt
where
	getUser uid tst							= accHStTSt (getUser uid) tst
	getUsers tst							= accHStTSt (getUsers) tst
	getUsersWithRole role tst				= accHStTSt (getUsersWithRole role) tst
	getDisplayNames	uids tst				= accHStTSt (getDisplayNames uids) tst
	getUserNames uids tst					= accHStTSt (getUserNames uids) tst
	getRoles uids tst						= accHStTSt (getRoles uids) tst
	authenticateUser username password tst	= accHStTSt (authenticateUser username password) tst
	