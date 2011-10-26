implementation module UserAdmin

import iTasks, Text, Time, Tuple, IWorld

from UserDB import qualified class UserDB(..)
from UserDB import qualified instance UserDB IWorld
from Shared import makeReadOnlyShared, :: SharedId, :: ReadWriteShared(..), :: SharedRead, :: SharedWrite, :: SharedGetTimestamp
from Util	import mb2error

users :: ReadOnlyShared [User]
users = makeReadOnlyShared "SystemData_users" 'UserDB'.getUsers 'UserDB'.lastChange

usersWithRole	:: !Role -> ReadOnlyShared [User]
usersWithRole role = makeReadOnlyShared ("SystemData_usersWithRole-" +++ toString role) ('UserDB'.getUsersWithRole role) 'UserDB'.lastChange

userDetails :: !User -> Shared UserDetails
userDetails user = ReadWriteShared ["userDetails-" +++ toString user] read write (appFst Ok o 'UserDB'.lastChange)
where
	read iworld	= appFst (mb2error "user not in database") ('UserDB'.getUserDetails user iworld)
	write details iworld
		# (_,iworld) = 'UserDB'.updateUser user details iworld
		= (Ok Void,iworld)

currentUserDetails :: ReadOnlyShared (Maybe UserDetails)
currentUserDetails = makeReadOnlyShared "SystemData_currentUserDetails" (\iworld=:{currentUser} -> 'UserDB'.getUserDetails currentUser iworld) (\iworld -> (Timestamp 0, iworld))
	
authenticateUser :: !String !String	-> Task (Maybe User)
authenticateUser username password = mkInstantTask ("Authenticate user", "Verify if there is a user with the supplied credentials.") eval
where
	eval taskNr iworld
		# (mbUser,iworld) = 'UserDB'.authenticateUser username password iworld
		= (TaskFinished mbUser,iworld)

createUser :: !UserDetails -> Task User
createUser user = mkInstantTask ("Create user", "Create a new user in the database.") eval
where
	eval taskNr iworld
	
		# (user,iworld) = 'UserDB'.createUser user iworld
		= case user of
			(Ok user)	= (TaskFinished user,iworld)
			(Error e)	= (taskException e, iworld)
			
deleteUser :: !User -> Task User
deleteUser user = mkInstantTask ("Delete user", "Delete a user from the database.") eval
where
	eval taskNr iworld
		# (user,iworld) = 'UserDB'.deleteUser user iworld
		= (TaskFinished user,iworld)
manageUsers :: Task Void
manageUsers =
	(		enterSharedChoice ("Users","The following users are available") [] users
		>?*	[ (Action "New",									Always	(createUserFlow			>>|	return False))
			, (ActionEdit,									IfValid (\u -> updateUserFlow u	>>|	return False))
			, (ActionDelete,								IfValid (\u -> deleteUserFlow u	>>|	return False))
			, (Action "Import & export/Import CSV file...",	Always	(importUserFileFlow		>>| return False))
			, (Action "Import & export/Export CSV file...",	Always	(exportUserFileFlow		>>| return False))
			, (Action "Import & export/Import demo users",	Always	(importDemoUsersFlow	>>| return False))
			, (ActionQuit,									Always	(							return True))
			]
	) <! id >>| return Void

createUserFlow :: Task Void
createUserFlow =
		enterInformation ("Create user","Enter user information") []
	>?*	[ (ActionCancel,	Always	(return Void))
		, (ActionOk,		IfValid (\user ->
											createUser user
										>>|	viewInformation "User created" [] "Successfully added new user"
										>>| return Void
									))
		]
		
updateUserFlow :: User -> Task User
updateUserFlow user  =
		get sharedDetails
	>>= \oldDetails -> updateInformation ("Editing " +++ displayName user,"Please make your changes") [] oldDetails
	>?*	[ (ActionCancel,	Always	(return user))
		, (ActionOk,		IfValid (\newDetails ->
											set newDetails sharedDetails
										>>=	viewInformation "User updated" [DisplayView (GetLocal (\{displayName} -> "Successfully updated " +++ displayName))]
										>>| return user
									))
		]
where
	sharedDetails = userDetails user
					
deleteUserFlow :: User -> Task User
deleteUserFlow user =
		viewInformation "Delete user" [] ("Are you sure you want to delete " +++ displayName user +++ "? This cannot be undone.")
	>?*	[ (ActionNo,	Always		(return user))
		, (ActionYes,	Always (		deleteUser user
									>>=	viewInformation "User deleted" [DisplayView (GetLocal (\user -> "Successfully deleted " +++ displayName user +++ "."))]
						))
		]
		
importUserFileFlow :: Task Void
importUserFileFlow = viewInformation "Not implemented" [] Void

exportUserFileFlow :: Task Document
exportUserFileFlow
	=	get users -&&- get applicationName
	>>= \(list,app) ->
		createCSVFile (app +++ "-users.csv") [toRow u \\ (RegisteredUser u) <- list] 
	>>=	viewInformation ("Export users file","A CSV file containing the users of this application has been created for you to download.") []
where
	toRow {username = (Username username), password = (Password password), displayName, emailAddress = (EmailAddress email), roles}
		= [displayName,username,password,email: fromMaybe [] roles]
	
importDemoUsersFlow :: Task [User]
importDemoUsersFlow =
	allTasks [catchAll (createUser (demoUser n)) (\_ -> return (RegisteredUser (demoUser n))) \\ n <- names]
where
	demoUser name
		= {UserDetails
		  | username = Username (toLowerCase name)
		  , password = Password (toLowerCase name)
		  , displayName = name
		  , emailAddress = EmailAddress (name +++ "@example.com")
		  , roles = Nothing
		  }
	names = ["Alice","Bob","Carol","Dave","Eve","Fred"]