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
										>>|	showMessage ("User created","Successfully added new user") [] Void
									))
		]
		
updateUserFlow :: User -> Task User
updateUserFlow user  =
		get sharedDetails
	>>= \oldDetails -> updateInformation ("Editing " +++ displayName user,"Please make your changes") [] oldDetails
	>?*	[ (ActionCancel,	Always	(return user))
		, (ActionOk,		IfValid (\newDetails ->
											set sharedDetails newDetails
										>>|	showMessage ("User updated","Successfully updated " +++ newDetails.displayName) [] user
									))
		]
where
	sharedDetails = userDetails user
					
deleteUserFlow :: User -> Task User
deleteUserFlow user =
		showMessage ("Delete user","Are you sure you want to delete " +++ displayName user +++ "? This cannot be undone.") [] Void
	>?*	[ (ActionNo,	Always		(return user))
		, (ActionYes,	Always (		deleteUser user
									>>=	showMessage ("User deleted","Successfully deleted " +++ displayName user +++ ".") []
						))
		]