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
	=   [ workflow "CEFP/Sect 5/1. Date and Time" 					"Shows current date and time"							(repeatUntilApproved (show getDateAndTime))
		, workflow "CEFP/Sect 5/2. Administrated users" 			"Shows currently administrated users"					(show getUsers)
		, workflow "CEFP/Sect 5/3. Administrated users details"		"Shows details of all currently administrated users"	(show getUsersDetails)
		, workflow "CEFP/Sect 5/4. Show details of a user"			"Select administrated user and show administration"		selectUserDetails 
		, workflow "CEFP/Sect 5/5. Current Workflows" 				"Which workflows are known here ?"						(show getWorkflows)
		, workflow "CEFP/Sect 5/6. To Do List" 						"Create and store a to do list"							(show updateMyStore)
		, workflow "CEFP/Sect 5/7. Shared To Do List" 				"Update a shared to do list"							(show updateMySharedStore)
		, workflow "CEFP/Sect 5/8. Show details of a user, vrs 2"	"Select administrated user and show administration"		selectUserDetails2
		, workflow "CEFP/Sect 5/9. To Do List, vrs 2" 				"Create and store a to do list" 						(show updateMyStore2)
		]

<<<<<<< .mine
=======
w1 = workflow "CEFP/Sect 5/1. Date and Time" 					"Shows current date and time"							(show getDateAndTime)
w2 = workflow "CEFP/Sect 5/2. Administrated users" 				"Shows currently administrated users"					(show getUsers)
w3 = workflow "CEFP/Sect 5/3. Administrated users details"		"Shows details of all currently administrated users"	(show getUsersDetails)
w4 = workflow "CEFP/Sect 5/4. Show details of a user"			"Select administrated user and show administration"		selectUserDetails 
w5 = workflow "CEFP/Sect 5/5. Current Workflows" 				"Which workflows are known here ?"						(show getWorkflows)
w6 = workflow "CEFP/Sect 5/6. To Do List" 						"Create and store a to do list"							(show updateToDoList)
w7 = workflow "CEFP/Sect 5/7. Show details of a user, vrs 2"	"Select administrated user and show administration"		selectUserDetails2
w8 = workflow "CEFP/Sect 5/8. To Do List, vrs 2" 				"Create and store a to do list" 						(show updateToDoList2)

>>>>>>> .r1715
// Date and Time

getDateAndTime :: Task DateTime
getDateAndTime
    =     		get currentDateTime

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

<<<<<<< .mine
:: ToDoList =	{ name :: String
				, deadline :: Maybe Date
				, remark :: Maybe Note
				, done :: Bool
=======
:: ToDo		=	{ name		:: !String
				, deadline	:: !Date
				, remark	:: !Maybe Note
				, done		:: !Bool
>>>>>>> .r1715
				}
derive class iTask ToDo

toDoList :: Shared [ToDo]
toDoList = sharedStore "My To Do List" []

updateToDoList :: Task [ToDo]
updateToDoList
    =     	get toDoList
      >>= 	updateInformation "Your To Do List" []
      >>=	set toDoList

updateMySharedStore :: Task [ToDoList]
updateMySharedStore
    =     	updateSharedInformation "Your To Do List" [] myStore
where
	myStore = sharedStore "My To Do List Store" [] 



// using interactions on shared data

selectUserDetails2 :: Task UserDetails
selectUserDetails2
    =     				enterSharedChoice "Select a user" [] users
      >>= \user -> 		get (userDetails user)
      >>= \details -> 	showInformation ("Details of user " <+++ user) [] details


updateToDoList2 :: Task [ToDo]
updateToDoList2
    =     	updateSharedInformation "Your To Do List" [] toDoList
