implementation module Groups

import iTasks

derive class iTask Group
derive bimap Maybe, (,)

instance DB Group
where
	databaseId 					= mkDBId "Groups"
	getItemId g					= DBRef g.Group.groupId
	setItemId (DBRef groupId) g	= {Group| g & groupId = groupId}

instance toString Group where toString g = g.Group.name

createGroup :: !String !User  -> Task Group
createGroup name user 
	= dbCreateItem {Group | groupId = 0, name = name, members = [user]}
		
getAllGroups :: Task [Group]
getAllGroups
	= dbReadAll 
 
getMyGroups :: Task [Group]
getMyGroups = getContextWorker >>= \user -> dbReadAll >>= transform (filter (groupMember user))
where
	groupMember user {Group|members}	= isMember user members

deleteGroup :: !Group -> Task Group
deleteGroup group = dbDeleteItem (getItemId group) >>| return group

addMemberToGroup :: !Group !User -> Task Group
addMemberToGroup group user
	= dbReadItem (getItemId group) >>= \mbGroup -> case mbGroup of
		Just group	= dbUpdateItem {Group|group & members = group.members ++ [user]}
		Nothing		= return group

removeMemberFromGroup :: !Group !User -> Task Group
removeMemberFromGroup group user
	= dbReadItem (getItemId group) >>= \mbGroup -> case mbGroup of
		//If the current user is the last user, delete the group
		Just group=:{members=[user]}	= deleteGroup group
		//Remove the user from the group
		Just group=:{members}			= dbUpdateItem {Group|group & members = removeMember user members}
		Nothing							= return group 
inviteUserToGroup :: !User !Group -> Task Group
inviteUserToGroup user group
	=	getContextWorker
	>>= \fromUser ->
		assign user (invite fromUser group)
	>>= \accept ->
		if accept
			(addMemberToGroup group user 
			 >>= showMessage "Invitation accepted" (toString user +++ " accepted your invitation to join the group " +++ toString group)
			)
			(showMessage "Invitation declined" (toString user +++ " declined your invitation to join the group " +++ toString group) group)
	>>= dbUpdateItem
where
	invite user group
		= requestConfirmation
			("Invitation to join group " +++ toString group)
			[Text (toString user +++ " invites you to join the group " +++ toString group +++ "."),BrTag [], Text "Do you accept this invitation?"]


manageGroups :: Task Void
manageGroups
	=	Subject "Manage groups" @>>
	(	getMyGroups
	>>=	overview 
	>>= \(action,group) -> case action of
		ActionNew	= newGroup >>= manageGroup 	>>| return False
		ActionOpen	= manageGroup group			>>| return False
		ActionQuit	= 								return True
	) <! id
	>>| return Void
where
	overview []		= getDefaultValue >>= showMessageA "My groups" "You have no groups." [aNew,aQuit]
	overview list	= enterChoiceA "My groups" "Select a group..." [aOpen,aNew,aQuit] list
	
	aOpen 			= ButtonAction (ActionOpen, IfValid)
	aNew			= ButtonAction (ActionNew, Always)
	aQuit			= ButtonAction (ActionQuit, Always)
	newGroup		= 		enterInformation "New group" "Please enter a name for the new group" 
						>>= \name ->
							getContextWorker
						>>= \user -> 
					  		createGroup name user
	
manageGroup :: Group -> Task Void
manageGroup group
	= 	showMessageAboutA (toString group) "This group contains the following members:" [aBack,aInvite,aLeave] group.Group.members
	>>= \(action,_) -> case action of
		ActionPrevious						= 					return Void
		ActionLabel "Invite a new member"	= invite group	>>| return Void
		ActionLabel "Leave group"			= leave group	>>| return Void
where
	aBack	= ButtonAction (ActionPrevious, Always)
	aInvite	= ButtonAction (ActionLabel "Invite a new member", Always)
	aLeave	= ButtonAction (ActionLabel "Leave group", Always)
		
	invite group
		= 	enterInformation ("Invite a someone to join " +++ toString group) "Please enter a user to invite to the group"
		>>=	\user ->
			inviteUserToGroup user group
			
	leave group
		=	getContextWorker
		>>= removeMemberFromGroup group
			

