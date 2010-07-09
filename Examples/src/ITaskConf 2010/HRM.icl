implementation module HRM

import iTasks

hrm :: [Workflow]
hrm = [restrictedWorkflow "Groups/Manage Groups" ["Chair"] manageGroups
	  ,restrictedWorkflow "Groups/List Groups" ["Chair"] listGroups
	  ]

:: Group = { label :: String, members :: (Maybe [User]) }

derive class iTask Group
derive bimap Maybe, (,)

manageGroups :: Task Void
manageGroups = buildGroups 
	>>= \groups 	-> updateInformation "Please assign users to groups" groups
	>>= \ngroups	-> getUsers
	>>= \users		-> removeAllGroupsFromUsers users
	>>|				   assignGroupsToUsers ngroups users

removeAllGroupsFromUsers :: [User] -> Task Void
removeAllGroupsFromUsers users = sequence "removeAllGroupsFromUsers" [removeAllGroupsFromUser u \\ u <- users] >>| return Void
where	
	removeAllGroupsFromUser :: User -> Task Void
	removeAllGroupsFromUser user =
		getUser (userName user) >>= \mbuser ->
			case mbuser of
				Just (RegisteredUser details=:{UserDetails | roles})
					= updateUser user {UserDetails | details & roles = Nothing} >>| return Void
				_
					= return Void
	
assignGroupsToUsers :: [Group] [User] -> Task Void	
assignGroupsToUsers groups users = sequence "assignGroupsToUsers" [assignGroupToUsers g users \\ g <- groups] >>| return Void
where
	assignGroupToUsers :: Group [User] -> Task Void
	assignGroupToUsers group users = sequence "assignGroupToUsers" [assignGroupToUser group u \\ u <- users] >>| return Void
	
	assignGroupToUser :: Group User -> Task Void
	assignGroupToUser group=:{label,members} user =
		getUser (userName user) >>= \mbuser ->
			case mbuser of
				Just (RegisteredUser details=:{UserDetails | roles})
					# roles = mb2list roles
					# members = mb2list members
					# details = case isMember user members of
						True  = {UserDetails | details & roles = list2mb (removeDup [label:roles])}
						False = details
					= updateUser user details >>| return Void
				_ 
					= return Void

listGroups :: Task Void
listGroups = buildGroups
	>>= showMessageAbout "This is the current assignment of roles"

getUserGroups :: Task [String]
getUserGroups = buildGroups
	>>= \groups = return [g.Group.label \\ g <- groups]

buildGroups :: Task [Group]
buildGroups = getUsers
	>>= \users -> return (foldl buildGroups` [] users)
where
	buildGroups` groups user 
		= case user of
			(RegisteredUser details=:{UserDetails | roles}) = foldl (addUserToGroups user) groups (mb2list roles)
			_ = groups
				
	addUserToGroups user groups role
		| isMember role [g.Group.label \\ g <- groups]  
			= [if(g.Group.label == role) {Group | g & members = list2mb (removeDup [user:mb2list g.members])} g \\ g <- groups]
		| otherwise
			= [{Group | label = role, members = Just [user]}:groups]