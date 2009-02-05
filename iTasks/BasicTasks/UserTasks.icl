implementation module UserTasks

//accNWorldHSt (accUserDBNWorld (authenticateUser username password)) hst

import TSt
import UserDB
import LiftingCombinators
import StdEnv, GenBimap


derive gForm []
derive gUpd []

getDisplayNamesTask :: ![Int] -> Task [String]
getDisplayNamesTask uids
	= appHStOnce "getDisplayNamesTask" (accNWorldHSt (accUserDBNWorld (getDisplayNames uids)))

getUserNamesTask :: ![Int] -> Task [String]
getUserNamesTask uids
	= appHStOnce "getUserNamesTask" (accNWorldHSt (accUserDBNWorld (getUserNames uids)))

getRolesTask :: ![Int]	-> Task [[String]]
getRolesTask uids
	= appHStOnce "getRolesTask" (accNWorldHSt (accUserDBNWorld (getRoles uids)))
	
getUsersWithRoleTask :: !String	-> Task [(Int,String)]
getUsersWithRoleTask role
	= appHStOnce "getUsersWithRoleTask" (accNWorldHSt (accUserDBNWorld (getUsersWithRole role)))

getUsersIds	::  (Task [Int])
getUsersIds
	= appHStOnce "getUsersIds" (accNWorldHSt (accUserDBNWorld getUserIds))
	
getCurrentUserId	::  Task Int
getCurrentUserId = mkBasicTask "getCurrentUserId" (Task getCurrentUser)
