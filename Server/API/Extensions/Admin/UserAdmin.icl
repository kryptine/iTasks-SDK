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
	overview []		= showMessageA "Users" "There are no users yet"
							[(ActionNew,always,AsButton),(ActionQuit,always,AsButton)] Nothing
	overview list	= enterChoiceA "Users" "The following users are available"
						[(ActionOpen,ifvalid,AsButton),(ActionDelete,ifvalid,AsButton),(ActionNew,always,AsButton), (ActionQuit,always,AsButton)] list
					>>= transform (\(a,i) -> (a,Just i))

createUserFlow :: Task Void
createUserFlow = Subject "Create user"
	@>>	enterInformationA "Create user" "Enter user information" [(ActionCancel, always, AsButton), (ActionOk, ifvalid, AsButton)]
	>>=	\(action,user) -> case action of
		ActionCancel	=	stop
		ActionOk		=	createUser user
						>>|	showMessage "User created" "Successfully added new user" Void

updateUserFlow :: User -> Task User
updateUserFlow user 
	= getUserDetails user
	>>= \(mbDetails) -> case mbDetails of
		Nothing
			= showMessage "Error" "Cannot update this user" user
		Just oldDetails 						
			= updateInformationA ("Editing " +++ displayName user)
			 	"Please make your changes" [(ActionCancel, always, AsButton), (ActionOk, ifvalid, AsButton)] oldDetails
			>>= \(action,newDetails) -> case action of
				ActionCancel	=	return user
				ActionOk		=	updateUser user newDetails >>= showMessage "User updated" ("Successfully updated " +++ newDetails.displayName)
									
deleteUserFlow :: User -> Task User
deleteUserFlow user
	=	requestConfirmation "Delete user" ("Are you sure you want to delete " +++ displayName user +++ "? This cannot be undone.")
	>>= \confirmation -> case confirmation of
		False	= return user
		True	= deleteUser user >>= showMessage "User deleted" ("Successfully deleted " +++ displayName user +++ ".")
 
