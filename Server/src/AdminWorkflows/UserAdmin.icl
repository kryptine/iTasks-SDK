implementation module UserAdmin

import iTasks

userAdministration :: [Workflow]
userAdministration
	= [ restrictedWorkflow "Admin/Create user" ["admin"] createUserFlow
	  , restrictedWorkflow "Admin/Update user" ["admin"] updateUserFlow
	  , restrictedWorkflow "Admin/Delete users" ["admin"] deleteUserFlow
	  , restrictedWorkflow "Admin/List users" ["admin"] listUserFlow
	  ]

createUserFlow :: Task Void
createUserFlow = Subject "Create user"
	@>>	enterInformationA "Create user" "Enter user information" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)]
	>>=	\(action,user) -> case action of
		ActionCancel	=	stop
		ActionOk		=	createUser user
						>>|	showMessage "User created" "Successfully added new user" Void

updateUserFlow :: Task Void
updateUserFlow = Subject "Update user" 
	@>>	getUsers
	>>= enterChoiceA "Choose user" "Which user do you want to update?" [ButtonAction (ActionCancel, Always), ButtonAction (ActionNext, IfValid)]
	>>= \(action1,user1 =:(RegisteredUser details1)) -> case action1 of
		ActionCancel	=	stop
		ActionNext		=	updateInformationA "Make changes" "Please make your changes" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] details1
						>>= \(action2,details2) -> case action2 of
							ActionCancel	=	stop
							ActionOk		=	updateUser user1 details2
											>>| showMessage "User updated" "Successfully updated user" Void
											
deleteUserFlow :: Task Void
deleteUserFlow = Subject "Delete users" 
	@>>	getUsers
	>>=	enterMultipleChoiceA "Choose users" "Which users do you want to delete?" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, Always)]
	>>= \(action,users) -> case action of
		ActionCancel	=	stop
		ActionOk		=	allTasks [deleteUser user \\ user <- users]
						>>| showMessage "User deleted" "Successfully deleted users" Void
						
listUserFlow :: Task Void
listUserFlow = Subject "List users"
	@>>	getUsers
	>>=	showMessageAbout "List users" "These are the current users"
	>>| stop
	