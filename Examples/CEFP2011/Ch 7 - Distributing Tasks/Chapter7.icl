implementation module Chapter7

// Examples showing distributed tasks

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows7 world

flows7 :: [Workflow]
flows7 =  [w1, w2, w3]

w1 = workflow "CEFP/Chap 7/1. Delegate a task" 	"Delegate a task to some administrated user" 	(delegate someTask)
w2 = workflow "CEFP/Chap 7/2. Guess" 			"Number guessing game" 							guessGame
w3 = workflow "CEFP/Chap 7/3. Appointment" 		"Try to make an appointment with several user" 	mkAppointment

// Delegating a task to someone

delegate :: (Task a) -> Task a | iTask a
delegate task
    =                	enterSharedChoice "Select a user" [] users
      >>= \user   -> 	user @: task
      >>= 				updateInformation "Check result" [] 

someTask :: Task Note
someTask = enterInformation "Enter Information" []

// Number guessing game

guessGame
      =                enterInformation "Please enter a number" []
        >>= \secret -> (guess secret -||- guess secret)
        >>= \winner -> showInformation (userName winner +++ " has won") [] winner
where
	guess secret = delegate (updateInformation "Guess a number" [] 0 <! (==) secret >>| get currentUser)


// Ask everyone if they can meet on acertain time and date

derive class iTask Enquire, YesNoBecause

:: Enquire	=	{ date 		:: Display Date
				, time 		:: Display Time
				, canJoin 	:: YesNoBecause
				}
:: YesNoBecause = Yes | No Note

mkAppointment ::  Task [Enquire]
mkAppointment 
    =                	enterInformation "Which date do you want to meet ?" []
      >>= \date ->		enterInformation "Which time do you want to meet ?" []
      >>= \time ->		enterSharedMultipleChoice "Which users need to join the meeting ?" [] users
      >>= \selected  -> mapTask  (\u -> u @: updateInformation "Can we meet ?" [] {date=Display date, time= Display time, canJoin=Yes}) selected
      >>= \answers ->	showInformation "Users answered" [] answers
      
mapTask :: (a -> Task b) [a] -> Task [b] | iTask b
mapTask f []  			
	= return []
mapTask f [a:as]  
	=				f a
		>>= \b ->	mapTask f as
		>>= \bs ->  return [b:bs]

//	Utility functions:
showAllUserNames :: Task [String]
showAllUserNames
    =            get users
      >>= \us -> let showUsers = map displayName us in
      				showInformation "The current users are: " [] showUsers

selectUser :: Task User
selectUser
    =     enterSharedChoice "Select a user:" [] users

selectUsers :: Task [User]
selectUsers
	=     enterSharedMultipleChoice "Select users:" [] users
