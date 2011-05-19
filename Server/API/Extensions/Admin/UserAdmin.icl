implementation module UserAdmin

import iTasks

manageUsers :: Task Void
manageUsers = 
	(		get users
		>>=	overview
		>>=	\(action,item) -> case action of
			ActionNew		= createUserFlow				>>| return False
			ActionOpen		= updateUserFlow (fromJust item)>>| return False
			ActionDelete	= deleteUserFlow (fromJust item)>>|	return False
			ActionQuit		= 									return True	
	) <! id >>| stop
where
	overview list = enterChoiceA ("Users","The following users are available") id
		(\mbU -> [(ActionOpen,maybe Nothing (\u -> Just (ActionOpen, Just u)) mbU),(ActionDelete,maybe Nothing (\u -> Just (ActionDelete, Just u)) mbU),(ActionNew,Just (ActionNew,Nothing)),(ActionQuit,Just (ActionQuit,Nothing))]) list

createUserFlow :: Task Void
createUserFlow = Title "Create user"
	@>>	enterInformationA ("Create user","Enter user information") (\mbUser -> [(ActionCancel,Just Nothing), (ActionOk,fmap Just mbUser)])
	>>=	\mbUser -> case mbUser of
		Just user	=		createUser user
						>>|	showMessage ("User created","Successfully added new user") Void
		_			= stop
		
updateUserFlow :: User -> Task User
updateUserFlow user 
	= get (userDetails user)
	>>= \oldDetails -> updateInformationA ("Editing " +++ displayName user,"Please make your changes")
							idView (\mbD -> [(ActionCancel, Just Nothing), (ActionOk, fmap Just mbD)]) oldDetails
	>>= \res -> case res of
		(Just newDetails)	= set (userDetails user) newDetails >>| showMessage ("User updated","Successfully updated " +++ newDetails.displayName) user
		_					= return user					
deleteUserFlow :: User -> Task User
deleteUserFlow user
	=	requestConfirmation ("Delete user","Are you sure you want to delete " +++ displayName user +++ "? This cannot be undone.")
	>>= \confirmation -> case confirmation of
		False	= return user
		True	= deleteUser user >>= showMessage ("User deleted","Successfully deleted " +++ displayName user +++ ".")
 
