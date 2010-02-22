implementation module UserAdmin

import iTasks

userAdministration :: [Workflow]
userAdministration
	= [{Workflow|name = "Admin/Create user", label = "Create user ", roles = ["admin"], mainTask = createUserFlow}
	  ,{Workflow|name = "Admin/Update user", label = "Update user", roles = ["admin"], mainTask = updateUserFlow}
	  ,{Workflow|name = "Admin/Delete users", label = "Delete users", roles = ["admin"], mainTask = deleteUserFlow}
	  ]

createUserFlow :: Task Void
createUserFlow
	=	enterInformationA "Enter user information" [ActionCancel] [ActionOk] []
	>>=	\(action,user) -> case action of
		ActionCancel	=	stop
		ActionOk		=	createUser user
						>>|	showMessage "Successfully added new user"

updateUserFlow :: Task Void
updateUserFlow
	=	getUsers
	>>= enterChoiceA "Which user do you want to update?" [ActionCancel] [ActionNext] []
	>>= \(action1,user1) -> case action1 of
		ActionCancel	=	stop
		ActionNext		=	updateInformationA "Please make your changes" [ActionCancel] [ActionOk] [] user1
						>>= \(action2,user2) -> case action2 of
							ActionCancel	=	stop
							ActionOk		=	updateUser user2
											>>| showMessage "Successfully updated user"
											
deleteUserFlow :: Task Void
deleteUserFlow
	=	getUsers
	>>=	enterMultipleChoiceA "Which users do you want to delete?" [ActionCancel,ActionOk] []
	>>= \(action,users) -> case action of
		ActionCancel	=	stop
		ActionOk		=	allTasks [deleteUser user \\ user <- users]
						>>| showMessage "Successfully deleted users"
	