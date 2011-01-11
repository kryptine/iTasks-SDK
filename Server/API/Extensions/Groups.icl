implementation module Groups

import iTasks

derive class iTask Group
derive bimap Maybe, (,)

instance DB Group
where
	databaseId 					= mkDBId "Groups"
	getItemId g					= DBRef (fromHidden g.Group.groupId)
	setItemId (DBRef groupId) g	= {Group| g & groupId = toHidden groupId}

instance toString Group where toString g = g.Group.name

manageGroups :: Task Void
manageGroups
	=	Title "Manage groups" @>>
	(	getMyGroups
	>>=	overview 
	>>= \(action,group) -> case fst action of
		ActionNew	= newGroup >>= manageGroup 	>>| return False
		ActionOpen	= manageGroup group			>>| return False
		ActionQuit	= 								return True
	) <! id
	>>| return Void
where
	overview []		= getDefaultValue >>= showMessageA ("My groups",startMsg) [aNew,aQuit]
	overview list	= enterChoiceA ("My groups",listMsg) [aOpen,aNew,aQuit] list
	
	aOpen 			= (ActionOpen, ifvalid)
	aNew			= (ActionNew, always)
	aQuit			= (ActionQuit, always)
	newGroup		= 		enterInformation ("New group","Please enter a name for the new group")
						>>= \name ->
							getContextWorker
						>>= \user -> 
					  		createGroup name user
	
	startMsg		= [Text "You have no groups yet.",BrTag [], BrTag []
					  ,Text "You can create your own user groups to which you can invite other users", BrTag []
					  ,Text "Members of a group can easily send each other messages "
					  ,Text "or ask each others opinions."
					  ]
					  
	listMsg			= [Text "You are a member of the groups listed below.", BrTag [], BrTag []
					  ,Text "You may select one to view it or create a new group."
					  ]
	
manageGroup :: Group -> Task Void
manageGroup igroup
	= 	
	(	justdo (dbReadItem (getItemId igroup))
	>>= \group ->
		showMessageAboutA (toString group,"This group contains the following members:") id [aBack,aInvite,aLeave] group.Group.members
	>>= \(action,_) -> case fst action of
		ActionClose					= 					return True
		Action "invite" _			= invite group	>>| return False
		Action "leave" _			= leave group	>>| return False
	) <! id >>| stop
where
	aBack	= (ActionClose, always)
	aInvite	= (Action "invite" "Invite new member", always)
	aLeave	= (Action "leave" "Leave group", always)
		
	invite group
		= 	enterInformation ("Invite a someone to join " +++ toString group,"Please enter a user to invite to the group")
		>>=	inviteUserToGroup group
			
	leave group
		=	getContextWorker
		>>= removeMemberFromGroup group

createGroup :: !String !User  -> Task Group
createGroup name user 
	= dbCreateItem {Group | groupId = Hidden 0, name = name, members = [user]}
		
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
		Just group	= dbUpdateItem {Group|group & members = removeDup (group.members ++ [user])}
		Nothing		= return group

removeMemberFromGroup :: !Group !User -> Task Group
removeMemberFromGroup group user
	= dbReadItem (getItemId group) >>= \mbGroup -> case mbGroup of
		//If the current user is the last user, delete the group
		Just group=:{members=[user]}	= deleteGroup group
		//Remove the user from the group
		Just group=:{members}			= dbUpdateItem {Group|group & members = removeMember user members}
		Nothing							= return group
		
inviteUserToGroup :: !Group !User -> Task Group
inviteUserToGroup group user
	=	getContextWorker
	>>= \fromUser ->
		spawnProcess True True (
			assign user (invite fromUser group)
		>>= \accept ->
			if accept
				(addMemberToGroup group user 
				 >>= showMessage ("Invitation accepted",toString user +++ " accepted your invitation to join the group " +++ toString group)
				)
				(showMessage ("Invitation declined",toString user +++ " declined your invitation to join the group " +++ toString group) group)
		)
	>>| showMessage ("Invitation sent","An invitation to join the group has been sent to " +++ toString user) group
where
	invite user group
		= requestConfirmation (
			"Invitation to join group " +++ toString group,
			[Text (toString user +++ " invites you to join the group " +++ toString group +++ "."),BrTag [], Text "Do you accept this invitation?"])


			

