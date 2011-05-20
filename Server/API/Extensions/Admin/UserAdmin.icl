implementation module UserAdmin

import iTasks

manageUsers :: Task Void
manageUsers =
	(		enterSharedChoiceA ("Users","The following users are available") id users
		>?*	[ (ActionNew,		Always	(			createUserFlow		>>|	return False))
			, (ActionOpen,		IfValid (\user -> 	updateUserFlow user	>>|	return False))
			, (ActionDelete,	IfValid (\user ->	deleteUserFlow user	>>|	return False))
			, (ActionQuit,		Always	(									return True))
			]
	) <! id >>| stop

createUserFlow :: Task Void
createUserFlow =
		enterInformationA ("Create user","Enter user information")
	>?*	[ (ActionCancel,	Always	stop)
		, (ActionOk,		IfValid (\user ->
											createUser user
										>>|	showMessage ("User created","Successfully added new user") Void
									))
		]
		
updateUserFlow :: User -> Task User
updateUserFlow user  =
		get sharedDetails
	>>= \oldDetails -> updateInformationA ("Editing " +++ displayName user,"Please make your changes") idView oldDetails
	>?*	[ (ActionCancel,	Always	(return user))
		, (ActionOk,		IfValid (\newDetails ->
											set sharedDetails newDetails
										>>|	showMessage ("User updated","Successfully updated " +++ newDetails.displayName) user
									))
		]
where
	sharedDetails = userDetails user
					
deleteUserFlow :: User -> Task User
deleteUserFlow user =
		showMessageA ("Delete user","Are you sure you want to delete " +++ displayName user +++ "? This cannot be undone.")
	>>*	[ (ActionNo,			return user)
		, (ActionYes,			deleteUser user
							>>=	showMessage ("User deleted","Successfully deleted " +++ displayName user +++ ".")
						)
		]