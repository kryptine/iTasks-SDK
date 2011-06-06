implementation module UserAdmin

import iTasks

manageUsers :: Task Void
manageUsers =
	(		enterSharedChoice ("Users","The following users are available") [] users
		>?*	[ (ActionNew,		Always	(			createUserFlow		>>|	return False))
			, (ActionOpen,		IfValid (\user -> 	updateUserFlow user	>>|	return False))
			, (ActionDelete,	IfValid (\user ->	deleteUserFlow user	>>|	return False))
			, (ActionQuit,		Always	(									return True))
			]
	) <! id >>| stop

createUserFlow :: Task Void
createUserFlow =
		enterInformation ("Create user","Enter user information") []
	>?*	[ (ActionCancel,	Always	stop)
		, (ActionOk,		IfValid (\user ->
											createUser user
										>>|	showInformation "User created" [] "Successfully added new user"
										>>| return Void
									))
		]
		
updateUserFlow :: User -> Task User
updateUserFlow user  =
		get sharedDetails
	>>= \oldDetails -> updateInformation ("Editing " +++ displayName user,"Please make your changes") [] oldDetails
	>?*	[ (ActionCancel,	Always	(return user))
		, (ActionOk,		IfValid (\newDetails ->
											set sharedDetails newDetails
										>>=	showInformation "User updated" [Get (\{displayName} -> "Successfully updated " +++ displayName)]
										>>| return user
									))
		]
where
	sharedDetails = userDetails user
					
deleteUserFlow :: User -> Task User
deleteUserFlow user =
		showInformation "Delete user" [] ("Are you sure you want to delete " +++ displayName user +++ "? This cannot be undone.")
	>?*	[ (ActionNo,	Always		(return user))
		, (ActionYes,	Always (		deleteUser user
									>>=	showInformation "User deleted" [Get (\user -> "Successfully deleted " +++ displayName user +++ ".")]
						))
		]