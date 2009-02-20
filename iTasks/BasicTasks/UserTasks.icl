implementation module UserTasks

import StdEnv, GenBimap
import TSt
import UserDB
import BasicCombinators, LiftingCombinators

getDisplayNamesTask :: ![Int] -> Task [String]
getDisplayNamesTask uids
	= appHStOnce "getDisplayNamesTask" (getDisplayNames uids)

getUserNamesTask :: ![Int] -> Task [String]
getUserNamesTask uids
	= appHStOnce "getUserNamesTask" (getUserNames uids)

getRolesTask :: ![Int]	-> Task [[String]]
getRolesTask uids
	= appHStOnce "getRolesTask" (getRoles uids)
	
getUsersWithRoleTask :: !String	-> Task [(Int,String)]
getUsersWithRoleTask role
	= appHStOnce "getUsersWithRoleTask" (getUsersWithRole role)

getUsersIds	:: Task [Int]
getUsersIds
	= appHStOnce "getUsersIds" getUserIds
	
getCurrentUserId :: Task Int
getCurrentUserId
	= once "getCurrentUserId" (Task getCurrentUser)
