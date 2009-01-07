definition module UserDB

import StdMaybe

:: UserDB 

openUserDB			:: 					!*World	->	(!*UserDB						, !*World)

getDisplayNames		:: ![Int]			!*UserDB -> (![String]						, !*UserDB)
getUserNames		:: ![Int]			!*UserDB -> (![String]						, !*UserDB)
getRoles			:: ![Int]			!*UserDB -> (![[String]]					, !*UserDB)
getUsersWithRole	:: !String			!*UserDB -> (![(Int,String)]				, !*UserDB)
authenticateUser	:: !String !String	!*UserDB -> (!Maybe (Int,String,[String])	, !*UserDB)