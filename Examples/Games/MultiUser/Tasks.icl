implementation module MultiUser.Tasks

import iTasks
import iTasks.Extensions.Admin.UserAdmin

startMultiUserTasks :: [Workflow] [StartableTask] *World -> *World
startMultiUserTasks workflows tasks world
	= startTask [ workflow "Manage users"  "Manage system users..."   manageUsers
				: workflows
				] tasks world

startTask taskList tasks world
	= startEngine [ publish "/" (\_-> browseExamples taskList)
                  : tasks
				  ] world
where
	browseExamples taskList = forever (
		 	enterInformation "Enter your credentials and login or press continue to remain anonymous" []
		>>* [OnAction (Action "Login") (hasValue (browseAuthenticated taskList))
			] )

	browseAuthenticated taskList {Credentials|username,password}
		= authenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorklist taskList)
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return ()
