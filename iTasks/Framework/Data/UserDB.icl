implementation module UserDB

import StdEnv
import StdMaybe

:: User 	= { uid			:: Int
			  , username	:: String
			  , password	:: String
			  , displayname	:: String
			  , roles		:: [String]
			  }
			  
:: UserDB	= { cache		:: [User]	//Cache must be ordered on ascending uids
			  }

//Dummy database with users used in some examples
dummyCache	= [ {User | uid = 0, username = "root", displayname = "Root", password = "", roles = ["president","manager","worker"]}
			  , {User | uid = 1, username = "president", displayname = "President", password = "", roles = ["president"]}
			  , {User | uid = 2, username = "manager", displayname = "Middle manager", password = "", roles = ["manager"]}
			  , {User | uid = 3, username = "worker1", displayname = "Office worker 1", password = "", roles = ["worker"]}
			  
			  , {User | uid = 10, username = "customer", displayname = "Customer", password = "", roles = ["customer"]}
			  , {User | uid = 11, username = "bank", displayname = "Bank authorization", password = "", roles = ["bank"]}
			  , {User | uid = 12, username = "storage", displayname = "Webshop storage", password = "", roles = ["storage"]}
			  , {User | uid = 13, username = "creditcard", displayname = "Creditcard authorization", password = "", roles = ["creditcard"]}    
			  
			  , {User | uid = 20, username = "megastore", displayname = "Megastore", password = "", roles = ["supplier"]}
			  , {User | uid = 21, username = "localshop", displayname = "Local shop", password = "", roles = ["supplier"]}
			  , {User | uid = 22, username = "webshop", displayname = "Webshop.com", password = "", roles = ["supplier"]}   
			  ]

openUserDB :: !*World -> (!*UserDB, !*World)
openUserDB world = ({UserDB | cache = dummyCache }, world)

getDisplayNames	:: ![Int] !*UserDB -> (![String], !*UserDB)
getDisplayNames	uids db=:{cache}
	= (map (lookupUserProperty cache (\u -> u.displayname) "Unknown user") uids, {db & cache = cache})

getUserNames :: ![Int] !*UserDB -> (![String], !*UserDB)
getUserNames uids db=:{cache}
	= (map (lookupUserProperty cache (\u -> u.username) "") uids, {db & cache = cache})
	
getRoles :: ![Int] !*UserDB -> (![[String]], !*UserDB)
getRoles uids db=:{cache}
	= (map (lookupUserProperty cache (\u -> u.roles) []) uids, {db & cache = cache})
	
getUserIds :: !*UserDB -> (![Int], !*UserDB)
getUserIds db=:{cache}
	= ([user.uid \\ user <- cache], {db & cache = cache})

//Helper function which finds a property of a certain user
lookupUserProperty :: ![User] !(User -> a) !a !Int -> a
lookupUserProperty users selectFunction defaultValue userId
		= case [selectFunction user \\ user <- users | user.uid == userId] of
			[x] = x
			_	= defaultValue

getUsersWithRole :: !String !*UserDB -> (![(Int,String)], !*UserDB)
getUsersWithRole role db=:{cache}
	= ([(user.uid,user.displayname) \\ user <- cache | isMember role user.roles], {db & cache = cache})

authenticateUser :: !String !String	!*UserDB -> (!Maybe (Int,String,[String]), !*UserDB)
authenticateUser username password db =:{cache}
	= case [u \\ u <- cache | u.username == username && u.password == password] of
		[user]	= (Just (user.uid, user.displayname, user.roles), {db & cache = cache})		
		_		= (Nothing, {db & cache = cache})

