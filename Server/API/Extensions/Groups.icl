implementation module Groups

import iTasks

derive class iTask Group
derive bimap Maybe, (,)

instance DB Group
where
	databaseId 					= sharedStore "Groups" []
	getItemId g					= DBRef (fromHidden g.Group.groupId)
	setItemId (DBRef groupId) g	= {Group| g & groupId = toHidden groupId}

instance toString Group where toString g = g.Group.name

manageGroups :: Task Void
manageGroups
	=	Title "Manage groups" @>>
	(	getMyGroups
	>>=	overview 
	>>= \res -> case res of
		(ActionOpen,Just group)	= manageGroup group			>>| return False
		(ActionNew,_)			= newGroup >>= manageGroup 	>>| return False
		(ActionQuit,_)			= 								return True
	) <! id
	>>| return Void
where
	overview []		= showInformation ("My groups",startMsg) [] Void >>+ \_ -> UserActions [(ActionNew,Just (ActionNew,Nothing)),(ActionQuit,Just (ActionQuit,Nothing))]
	overview list	= enterChoice ("My groups",listMsg) [] list >>+ (\{modelValue,localValid} -> let mbG = if localValid (Just modelValue) Nothing in UserActions [aOpen mbG,aNew,aQuit])
	
	aOpen mbG		= (ActionOpen, maybe Nothing (\g -> Just (ActionOpen,Just g)) mbG)
	aNew			= (ActionNew, Just (ActionNew,Nothing))
	aQuit			= (ActionQuit, Just (ActionQuit,Nothing))
	newGroup		= 		enterInformation ("New group","Please enter a name for the new group") []
						>>= \name ->
							get currentUser
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
		showInformation (toString group,"This group contains the following members:") [Get id] group.members >>+ (\_ -> UserActions [(aBack,Just aBack),(aInvite,Just aInvite),(aLeave,Just aLeave)])
	>>= \action -> case action of
		ActionClose					= 					return True
		Action "invite" _			= invite group	>>| return False
		Action "leave" _			= leave group	>>| return False
	) <! id >>| stop
where
	aBack	= ActionClose
	aInvite	= Action "invite" "Invite new member"
	aLeave	= Action "leave" "Leave group"
		
	invite group
		= 	enterInformation ("Invite a someone to join " +++ toString group,"Please enter a user to invite to the group") []
		>>=	inviteUserToGroup group
			
	leave group
		=	get currentUser
		>>= removeMemberFromGroup group

createGroup :: !String !User  -> Task Group
createGroup name user 
	= dbCreateItem {Group | groupId = Hidden 0, name = name, members = [user]}
		
getAllGroups :: Task [Group]
getAllGroups
	= dbReadAll 
 
getMyGroups :: Task [Group]
getMyGroups = get currentUser >>= \user -> dbReadAll >>= transform (filter (groupMember user))
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
	=	get currentUser
	>>= \fromUser ->
		spawnProcess True initManagerProperties noMenu (
			user @: (invite fromUser group)
		>>= \accept ->
			if accept
				(addMemberToGroup group user 
				 >>= showInformation ("Invitation accepted",toString user +++ " accepted your invitation to join the group " +++ toString group) []
				)
				(showInformation ("Invitation declined",toString user +++ " declined your invitation to join the group " +++ toString group) [] group)
		)
	>>| showInformation ("Invitation sent","An invitation to join the group has been sent to " +++ toString user) [] group
where
	invite user group
		= showInformation (
			"Invitation to join group " +++ toString group,
			[Text (toString user +++ " invites you to join the group " +++ toString group +++ "."),BrTag [], Text "Do you accept this invitation?"])
			[] Void
			>>+ \_ -> UserActions [(ActionNo,Just False),(ActionYes,Just True)]