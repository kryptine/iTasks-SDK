implementation module UserAdmin

import iTasks

userAdministration :: [Workflow]
userAdministration
	= [{Workflow|name = "Admin/Create user", label = "Create user ", roles = ["admin"], mainTask = createUserFlow}
	  ,{Workflow|name = "Admin/Update user", label = "Update user", roles = ["admin"], mainTask = updateUserFlow}
	  ,{Workflow|name = "Admin/Delete user", label = "Delete user", roles = ["admin"], mainTask = deleteUserFlow}
	  ]

createUserFlow :: Task Void
createUserFlow
	=	enterInformationA "Enter user information" [ActionCancel] [ActionOk]
	>>=	\(action,user) -> case action of
			ActionCancel	=	stop
			ActionOk		=	createUser user
							>>|	showMessage "Successfully added new user"

updateUserFlow :: Task Void
updateUserFlow
	=	getUsers
	>>= enterChoice "Which user do you want to update?"
	>>= updateInformation "Please make your changes"
	>>= updateUser
	>>| showMessage "Successfully updated user"

deleteUserFlow :: Task Void
deleteUserFlow
	=	getUsers
	>>=	enterChoice "Which user do you want to delete?"
	>>= deleteUser
	>>| showMessage "Successfully deleted user"