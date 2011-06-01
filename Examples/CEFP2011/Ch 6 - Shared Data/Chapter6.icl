implementation module Chapter6

// Examples showing the usage of shared data

// Prdefined shared system variables can be found in: SystemData.dcl

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows6 world

flows6 :: [Workflow]
flows6 =  [w1, w2, w3, w4, w6]

w1 = workflow "CEFP/Chap 5/1. Date and Time" 				"Shows current date and time" 						(show getDateAndTime)
w2 = workflow "CEFP/Chap 5/2. Administrated users" 			"Shows currently administrated users" 				(show getUsers)
w3 = workflow "CEFP/Chap 5/3. Administrated users details"	"Shows details of currently administrated users"	(show getUserDetails)
w4 = workflow "CEFP/Chap 5/4. Current Workflows" 			"Which workflows are know here ?" 					(show getWorkflows)
w6 = workflow "CEFP/Chap 5/6. To Do List" 				    "Create and store a to do list" 					(show updateMyStore)

show :: (Task a) -> Task a | iTask a
show task = task >>= \r -> showMessage "The result is:" [About r] r

// Date and Time

getDateAndTime :: Task DateTime
getDateAndTime
    =     		get currentDateTime

// Administrated users

getUsers :: Task [User]
getUsers
    =     		get users

// Administrated users details

getUserDetails :: Task [UserDetails]
getUserDetails
    =     		get users
      >>= 		getDetails
where
	getDetails []  = return []
	getDetails [user:users]
		=				get (userDetails user)
			>>= \d ->	getDetails users
			>>= \ds ->	return [d:ds]

// Administrated users details

getWorkflows :: Task [WorkflowDescription]
getWorkflows	
    =     		get workflows

// getWorkflow would be nice as example ...


// Make my own shared store

:: ToDoList =	{ name :: String
				, deadline :: Date
				, remark :: Maybe Note
				, done :: Bool
				}

derive class iTask ToDoList

updateMyStore :: Task [ToDoList]
updateMyStore
    =     	get myStore
      >>= 	updateInformation "Your To Do List" []
      >>=	set myStore
where
	myStore = sharedStore "My To Do List Store" [] 