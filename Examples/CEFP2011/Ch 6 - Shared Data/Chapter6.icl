implementation module Chapter6

// Examples showing the usage of shared data

// Prdefined shared system variables can be found in: SystemData.dcl

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows6 world

flows6 :: [Workflow]
flows6 =  [w1, w2, w3, w4, w6]

w1 = workflow "CEFP/Chap 5/1. Date and Time" 				"Shows current date and time" 						getDateAndTime
w2 = workflow "CEFP/Chap 5/2. Administrated users" 			"Shows currently administrated users" 				getUsers
w3 = workflow "CEFP/Chap 5/3. Administrated users details"	"Shows details of currently administrated users"	getUserDetails
w4 = workflow "CEFP/Chap 5/4. Current Workflows" 			"Which workflows are know here ?" 					getWorkflows
w6 = workflow "CEFP/Chap 5/6. Shared Store" 				"Make your own shared store" 						updateMyStore

show :: (Task a) -> Task a | iTask a
show task = task >>= showMessageAbout "The result is:"

// Date and Time

getDateAndTime :: Task DateTime
getDateAndTime
    =     		get currentDateTime
      >>= 		showMessageAbout "The current date and time is: "

// Administrated users

getUsers :: Task [User]
getUsers
    =     		get users
      >>= 		showMessageAbout "Currently the following users are administrated: "

// Administrated users details

getUserDetails :: Task [UserDetails]
getUserDetails
    =     		get users
      >>= 		getDetails
      >>=		showMessageAbout "Currently the following users are administrated: "
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
      >>=		showMessageAbout "Currently the following workflows are known: "

// getWorkflow would be nice as example ...


// Make my own shared store

updateMyStore :: Task [DateTime]
updateMyStore
    =     	get myStore
      >>= 	updateInformation "Change store content"
      >>=	set myStore
      >>=	showMessageAbout "I have just stored the following: "
where
	myStore = sharedStore "My DateAndTime Store" [] 