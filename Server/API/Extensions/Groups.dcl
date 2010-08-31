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
	{ groupId	:: !Hidden Int
	, name		:: !String
	, members	:: ![User]
	}

derive class iTask Group

instance DB Group
instance toString Group

/**
* Top level workflow for users to manage their own group
*
*/
manageGroups			:: 						Task Void
/**
* Management workflow for a single group.
*
* @param The group to manage
*/
manageGroup 			:: Group 			->	Task Void
/**
* Create a new group
*
* @param A name for the group
* @param The first member of the group
*
* @return The newly created group
*/
createGroup				:: !String !User	->	Task Group
/**
* Retrieve all groups that the current user is a member of.
*
* @return The list of groups
*/
getMyGroups				:: 						Task [Group]
/**
* Retrieve all groups
* 
* @return The list of groups
*/
getAllGroups			:: 						Task [Group] 
/**
* Delete a group from the system.
* This is not really neccesary because groups are automatically deleted when
* the last member is removed from them.
*
* @param The group to delete
*
* @return The deleted group
*/
deleteGroup				:: !Group			->	Task Group
/**
* Add a user to a group. Groups can not contain duplicates.
* 
* @param The group to add the user to
* @param The user to add to the group
*
* @return The (updated) group
*/
addMemberToGroup		:: !Group !User 	->	Task Group
/**
* Remove a user from a group.
* When the last user is removed it is deleted from the system.
*
* @param The group to remove the user from
* @param The user to remove from the group
*
* @return The (updated) group
*/
removeMemberFromGroup	:: !Group !User 	->	Task Group
/**
* Ask a user if (s)he wants to join the group.
* If the invitation is accepted the user is added to the group.
* If it is declined, the group is unchanged.
*
* @param The group to invite the user to
* @param The user to invite
*
* @param The (possibly) updated group
*/
inviteUserToGroup		:: !Group !User 	->	Task Group
