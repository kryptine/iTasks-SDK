implementation module Section5

// Examples showing the usage of shared data

// Prdefined shared system variables can be found in: SystemData.dcl

import iTasks
from Section3 import show, repeatUntilApproved

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows5 world

flows5 :: [Workflow]
flows5 
	=   [ workflow "CEFP/Section 5 - Shared Data/1. Date and Time" 					"Shows current date and time"							(show getDateAndTime)
		, workflow "CEFP/Section 5 - Shared Data/2. Continuous Date and Time"			"Keep showing date and time" 							repeatDateAndTime
		, workflow "CEFP/Section 5 - Shared Data/3. Administrated users" 				"Shows currently administrated users"					(show getUsers)
		, workflow "CEFP/Section 5 - Shared Data/4. Administrated users details"		"Shows details of all currently administrated users"	(show getUsersDetails)
		, workflow "CEFP/Section 5 - Shared Data/5. Show details of a user"			"Select administrated user and show administration"		selectUserDetails 
		, workflow "CEFP/Section 5 - Shared Data/6. Current Workflows" 				"Which workflows are known here ?"						(show getWorkflows)
		, workflow "CEFP/Section 5 - Shared Data/7. Update To Do List" 				"Edit local copy of To Do list"							(show updateToDoList)
		, workflow "CEFP/Section 5 - Shared Data/8. Update Shared To Do List " 		"Edit To Do list, and share it right away"				(show updateToDoList2)
		, workflow "CEFP/Section 5 - Shared Data/9. View the Shared To Do List" 		"View will be adjusted when updated elsewhere"			viewToDoList 
		, workflow "CEFP/Section 5 - Shared Data/10. Show details of a user, vrs 2"	"Select administrated user and show administration"		selectUserDetails2
		]
		
// Date and Time
getDateAndTime :: Task DateTime
getDateAndTime
    =     		get currentDateTime

repeatDateAndTime :: Task DateTime
repeatDateAndTime
	=			repeatUntilApproved (show getDateAndTime)



// Administrated users

getUsers :: Task [User]
getUsers
    =     		get users

// Administrated users details

getUsersDetails :: Task [UserDetails]
getUsersDetails
    =     		get users
      >>= 		getDetails
where
	getDetails []  = return []
	getDetails [user:users]
		=				get (userDetails user)
			>>= \d ->	getDetails users
			>>= \ds ->	return [d:ds]


selectUserDetails :: Task UserDetails
selectUserDetails
    =     				get users
      >>= \users ->		enterChoice "Select a user" [] users
      >>= \user -> 		get (userDetails user)
      >>= \details -> 	showInformation ("Details of user " <+++ user) [] details

// Administrated users details

getWorkflows :: Task [WorkflowDescription]
getWorkflows	
    =     		get workflows

// getWorkflow would be nice as example ...


// Make my own shared store

:: ToDo =	{ name     :: String
			, deadline :: Maybe Date
			, remark   :: Maybe Note
			, done     :: Bool
			}
derive class iTask ToDo

toDoList :: Shared [ToDo]
toDoList = sharedStore "My To Do List" []

updateToDoList :: Task [ToDo]
updateToDoList
    =     	get toDoList
      >>= 	updateInformation "Your To Do List" []
      >>=	set toDoList

updateToDoList2 :: Task ([ToDo], Void)
updateToDoList2
    =     	updateSharedInformation "Your To Do List" []  toDoList Void 


viewToDoList :: Task ([ToDo], Void)
viewToDoList
	=		showSharedInformation "Your To Do List" []  toDoList Void 



// using interactions on shared data

selectUserDetails2 :: Task UserDetails
selectUserDetails2
    =     				enterSharedChoice "Select a user" [] users
      >>= \user -> 		get (userDetails user)
      >>= \details -> 	showInformation ("Details of user " <+++ user) [] details


