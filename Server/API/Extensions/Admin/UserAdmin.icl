implementation module UserAdmin

import iTasks, Text

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
	toRow {userName, password = (Password password), displayName, emailAddress = (EmailAddress email), roles}
		= [displayName,userName,password,email: fromMaybe [] roles]
	
importDemoUsersFlow :: Task [User]
importDemoUsersFlow =
	allTasks [catchAll (createUser (demoUser n)) (\_ -> return (RegisteredUser (demoUser n))) \\ n <- names]
where
	demoUser name
		= {UserDetails
		  | userName = toLowerCase name
		  , password = Password (toLowerCase name)
		  , displayName = name
		  , emailAddress = EmailAddress (name +++ "@example.com")
		  , roles = Nothing
		  }
	names = ["Alice","Bob","Carol","Dave","Eve","Fred"]