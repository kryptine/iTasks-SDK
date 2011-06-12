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
		, workflow "CEFP/Section 5 - Shared Data/2. Continuous Date and Time"		"Keep showing date and time" 							showDateAndTime
		, workflow "CEFP/Section 5 - Shared Data/3. Administrated users" 			"Shows currently administrated users"					(show getUsers)
		, workflow "CEFP/Section 5 - Shared Data/4. Administrated users details"	"Shows details of all currently administrated users"	(show getUsersDetails)
		, workflow "CEFP/Section 5 - Shared Data/5. Show details of a user"			"Select administrated user and show administration"		selectUserDetails 
		, workflow "CEFP/Section 5 - Shared Data/6. Current Workflows" 				"Which workflows are known here ?"						(show getWorkflows)
		, workflow "CEFP/Section 5 - Shared Data/7. Update To Do List" 				"Edit local copy of To Do list"							(show updateToDoList)
		, workflow "CEFP/Section 5 - Shared Data/8. Update Shared To Do List " 		"Edit To Do list, and share it right away"				(show updateToDoList2)
		, workflow "CEFP/Section 5 - Shared Data/9. View the Shared To Do List" 	"View will be adjusted when updated elsewhere"			showToDoList 
		, workflow "CEFP/Section 5 - Shared Data/10. Twitter" 						"Follow a Tweet"										joinCEFPtweets  
		, workflow "CEFP/Section 5 - Shared Data/11. Show details of a user, vrs 2"	"Select administrated user and show administration"		selectUserDetails2
		]
		
// Date and Time
getDateAndTime :: Task DateTime
getDateAndTime
    =     		get currentDateTime

showDateAndTime :: Task (DateTime,Void)  // better switch on automatic refresh
showDateAndTime
	=			showSharedInformation "The current date and time is: " [] currentDateTime Void



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


showToDoList :: Task ([ToDo], Void)
showToDoList
	=		showSharedInformation "Your To Do List" []  toDoList Void 

// using interactions on shared data

selectUserDetails2 :: Task UserDetails
selectUserDetails2
    =     				enterSharedChoice "Select a user" [] users
      >>= \user -> 		get (userDetails user)
      >>= \details -> 	showInformation ("Details of user " <+++ user) [] details

// 

:: Tweet  :== (User,String)

twitterId :: String -> Shared [Tweet]
twitterId name  = sharedStore ("Twitter with " +++ name) []

joinCEFPtweets 
	= joinTweets "CEFP" (twitterId "CEFP")

joinTweets  :: String (Shared [Tweet]) -> Task Void
joinTweets name tweets
	=				enterSharedInformation ("Enter tweet for " +++ name) views tweets
		>?*			[(ActionQuit,Always (return Void))
					,(ActionOk, IfValid commitTweetAndContinue)
					]
where
	commitTweetAndContinue :: ([Tweet],String) -> Task Void
	commitTweetAndContinue (_, reaction) 
		=				get currentUser
			>>= \me ->	update (\tweets -> tweets ++ [(me,reaction)]) tweets 
			>>| 		joinTweets name tweets

	views = [ ShowView (GetShared id)
			, EnterView (PutbackLocal \(Note reaction) _ _ -> reaction)
			]