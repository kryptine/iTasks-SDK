implementation module UserTasks

import TSt
import UserDB
import BasicCombinators, LiftingCombinators

getDisplayNamesTask :: ![Int] -> Task [String]
getDisplayNamesTask uids
	= appHSt "getDisplayNamesTask" (getDisplayNames uids)

getUserNamesTask :: ![Int] -> Task [String]
getUserNamesTask uids
	= appHSt "getUserNamesTask" (getUserNames uids)

getRolesTask :: ![Int]	-> Task [[String]]
getRolesTask uids
	= appHSt "getRolesTask" (getRoles uids)
	
getUsersWithRoleTask :: !String	-> Task [(Int,String)]
getUsersWithRoleTask role
	= appHSt "getUsersWithRoleTask" (getUsersWithRole role)

getUsersIds	:: Task [Int]
getUsersIds
	= appHSt "getUsersIds" getUserIds
	
getCurrentUserId :: Task Int
getCurrentUserId
	= mkBasicTask "getCurrentUserId" getCurrentUser
