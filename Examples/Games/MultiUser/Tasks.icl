implementation module MultiUser.Tasks

import iTasks
import iTasks.Extensions.Admin.UserAdmin

startMultiUserTasks :: [Workflow] [StartableTask] *World -> *World
startMultiUserTasks workflows tasks world
	= startTask [ workflow "Manage users"  "Manage system users..."   manageUsers
				: workflows
				] tasks world

startTask taskList tasks world
	= doTasks [ onStartup (installWorkflows taskList)
	          , onRequest "/" browseExamples
              : tasks
              ] world
where
	browseExamples = forever (
		 	enterInformation "Enter your credentials and login or press continue to remain anonymous" []
		>>* [OnAction (Action "Login") (hasValue browseAuthenticated)
			] )

	browseAuthenticated {Credentials|username,password}
		= authenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user manageWorkOfCurrentUser
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return ()
