implementation module HRM

import iTasks

hrm :: [Workflow]
hrm = [restrictedWorkflow "HRM/Manage Human Resources" ["chair"] manageHumanResources
	  ,restrictedWorkflow "HRM/List Human Resources" ["chair"] listHumanResources
	  ]

derive gPrint HRTree
derive gParse HRTree
derive gVisualize HRTree
derive gUpdate HRTree
derive gHint HRTree

derive bimap Maybe, (,)

getUserRoles :: [String]
getUserRoles = ["chair","steering committee","program committee","secretary","catering","it support","av support"]

:: HRTree =
	{ chair 				:: User
	, steeringCommittee 	:: [User]
	, programCommittee		:: [User]
	, secretary				:: [User]
	, catering				:: [User]
	, itSupport				:: [User]
	, avSupport				:: [User]	
	}
	
gError{|HRTree|} hrt est
	# est = if(isEmpty hrt.steeringCommittee) (labeledChild "steeringCommittee" (appendError "Please select one or more users" MPAlways) est) est
	# est = if(isEmpty hrt.programCommittee)  (labeledChild "programCommittee" (appendError "Please select one or more users" MPAlways) est) est
	# est = if(isEmpty hrt.secretary) 		  (labeledChild "secretary" (appendError "Please select one or more users" MPAlways) est) est
	# est = if(isEmpty hrt.catering) 	      (labeledChild "catering" (appendError "Please select one or more users" MPAlways) est) est
	# est = if(isEmpty hrt.itSupport)         (labeledChild "itSupport" (appendError "Please select one or more users" MPAlways) est) est
	# est = if(isEmpty hrt.avSupport)         (labeledChild "avSupport" (appendError "Please select one or more users" MPAlways) est) est
	= stepOut est

listHumanResources :: Task Void
listHumanResources = buildTree >>= showMessageAbout "Assigned Roles"
	
manageHumanResources :: Task Void
manageHumanResources = updateTree
	>>= \hrTree -> assignRolesToUsers hrTree
	>>| showMessage "User roles updated"	
where	
	updateTree :: Task HRTree
	updateTree = buildTree >>= updateInformation "Please assign users to the various roles"

	assignRolesToUsers :: HRTree -> Task Void
	assignRolesToUsers hrtree = getUsers
		>>= \users -> sequence "Assign Roles to all users" [assignRolesToUser u hrtree \\ u <- users]
		>>| return Void
	where
		assignRolesToUser :: User HRTree -> Task Void
		assignRolesToUser user ht
			# assignments  = [("chair",[ht.chair]),("steering committee",ht.steeringCommittee),
				("program committee",ht.programCommittee),("secretary",ht.secretary),
				("catering",ht.catering),("it support",ht.itSupport),("av support",ht.avSupport)]
			= sequence ("Assign Roles to "+++toString user) [assignRoleToUser user a \\ a <- assignments] 
			>>| return Void
		
		assignRoleToUser :: User (Role,[User]) -> Task Void
		assignRoleToUser user (role,assignUsers) = 
			getUser (userName user) >>= \mbuser ->			
				case mbuser of
					Just (RegisteredUser details=:{UserDetails | roles})
						# roles	  = mb2list roles
						# details = case isMember user assignUsers of
										True = {UserDetails | details & roles = list2mb (removeDup [role:roles])}
										False = {UserDetails | details & roles = list2mb (removeMember role roles)}
						= updateUser user details >>| return Void
					_ 
						= return Void

buildTree :: Task HRTree
buildTree = getUsers
	>>= \users -> return (buildTree` users)
where
	buildTree` :: [User] -> HRTree
	buildTree` users =	(foldl mapUsers mkHrTree users)
	
	mapUsers tree user =
		case user of
			(RegisteredUser details=:{UserDetails | roles}) = foldl (fillTree user) tree (mb2list roles)
			_ = tree
		
	fillTree user tree role =
		case role of
			"chair" = {tree & chair = user}
			"steering committee" = {tree & steeringCommittee = [user:tree.steeringCommittee]}
			"program committee" = {tree & programCommittee = [user:tree.programCommittee]}
			"catering" = {tree & catering = [user:tree.catering]}
			"secretary" = {tree & secretary = [user:tree.secretary]}
			"it support" = {tree & itSupport = [user:tree.itSupport]}
			"av support" = {tree & avSupport = [user:tree.avSupport]}
	
	mkHrTree = {HRTree
		| chair = AnyUser
		, steeringCommittee 	= []
		, programCommittee		= []
		, secretary				= []
		, catering				= []
		, itSupport				= []
		, avSupport				= []	
		}