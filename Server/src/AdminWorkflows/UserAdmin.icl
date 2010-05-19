implementation module UserAdmin

import iTasks

userAdministration :: [Workflow]
userAdministration
	= [{Workflow|name = "Admin/Create user", roles = ["admin"], mainTask = "Create user " @>> createUserFlow}
	  ,{Workflow|name = "Admin/Update user", roles = ["admin"], mainTask = "Update user" @>> updateUserFlow}
	  ,{Workflow|name = "Admin/Delete users", roles = ["admin"], mainTask = "Delete users" @>> deleteUserFlow}
	  ,{Workflow|name = "Admin/List users", roles = ["admin"], mainTask = "List users" @>> listUserFlow}
	  ]

createUserFlow :: Task Void
createUserFlow
	=	enterInformationA "Enter user information" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)]
	>>=	\(action,user) -> case action of
		ActionCancel	=	stop
		ActionOk		=	createUser user
						>>|	showMessage "Successfully added new user"

updateUserFlow :: Task Void
updateUserFlow
	=	getUsers
	>>= enterChoiceA "Which user do you want to update?" [ButtonAction (ActionCancel, Always), ButtonAction (ActionNext, IfValid)]
	>>= \(action1,user1) -> case action1 of
		ActionCancel	=	stop
		ActionNext		=	updateInformationA "Please make your changes" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] user1
						>>= \(action2,user2) -> case action2 of
							ActionCancel	=	stop
							ActionOk		=	updateUser user2
											>>| showMessage "Successfully updated user"
											
deleteUserFlow :: Task Void
deleteUserFlow
	=	getUsers
	>>=	enterMultipleChoiceA "Which users do you want to delete?" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, Always)]
	>>= \(action,users) -> case action of
		ActionCancel	=	stop
		ActionOk		=	allTasks [deleteUser user \\ user <- users]
						>>| showMessage "Successfully deleted users"
						
listUserFlow :: Task Void
listUserFlow
	=	getUsers
	>>=	showMessageAbout "These are the current users"
	