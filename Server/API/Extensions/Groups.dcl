definition module Groups
/**
* This extension provides the possibility to create groups of users.
*
* - Users can create new groups (they automatically become member)
* - Group members can invite others to join a group
* - Group members can leave a group (when the last member is removed from a group it is deleted)
*/

import iTasks

:: Group =
	{ groupId	:: !Int
	, name		:: !String
	, members	:: ![User]
	}

derive class iTask Group

instance DB Group
instance toString Group

manageGroups			:: 						Task Void
manageGroup 			:: Group 			->	Task Void

createGroup				:: !String !User	->	Task Group
getAllGroups			:: 						Task [Group] 
getMyGroups				:: 						Task [Group]
deleteGroup				:: !Group			->	Task Group

addMemberToGroup		:: !Group !User 	->	Task Group
removeMemberFromGroup	:: !Group !User 	->	Task Group

inviteUserToGroup		:: !User !Group		->	Task Group

