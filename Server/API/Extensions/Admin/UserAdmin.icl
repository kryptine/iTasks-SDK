implementation module UserAdmin

import iTasks

manageUsers :: Task Void
manageUsers = 
	(		getUsers
		>>= overview
		>>= \(action,item) -> case action of
			ActionNew		= createUserFlow				>>| return False
			ActionOpen		= updateUserFlow (fromJust item)>>| return False
			ActionDelete	= deleteUserFlow (fromJust item)>>|	return False
			ActionQuit		= 									return True	
	) <! id >>| stop
where
	overview []		= showMessageA ("Users","There are no users yet")
							[(ActionNew,always),(ActionQuit,always)] Nothing
	overview list	= enterChoiceA ("Users","The following users are available") id
						[(ActionOpen,ifvalid),(ActionDelete,ifvalid),(ActionNew,always), (ActionQuit,always)] list

createUserFlow :: Task Void
createUserFlow = Title "Create user"
	@>>	enterInformationA ("Create user","Enter user information") id [(ActionCancel, always), (ActionOk, ifvalid)]
	>>=	\res -> case res of
		(ActionOk,Just user)	=	createUser user
									>>|	showMessage ("User created","Successfully added new user") Void
		(ActionCancel,_)		= stop
		
updateUserFlow :: User -> Task User
updateUserFlow user 
	= getUserDetails user
	>>= \(mbDetails) -> case mbDetails of
		Nothing
			= showMessage ("Error","Cannot update this user") user
		Just oldDetails 						
			= updateInformationA ("Editing " +++ displayName user,"Please make your changes")
					idView [(ActionCancel, always), (ActionOk, ifvalid)] oldDetails
			>>= \res -> case res of
				(ActionOk,Just newDetails)	= updateUser user newDetails >>= showMessage ("User updated","Successfully updated " +++ newDetails.displayName)
				(ActionCancel,_)			= return user					
deleteUserFlow :: User -> Task User
deleteUserFlow user
	=	requestConfirmation ("Delete user","Are you sure you want to delete " +++ displayName user +++ "? This cannot be undone.")
	>>= \confirmation -> case confirmation of
		False	= return user
		True	= deleteUser user >>= showMessage ("User deleted","Successfully deleted " +++ displayName user +++ ".")
 
