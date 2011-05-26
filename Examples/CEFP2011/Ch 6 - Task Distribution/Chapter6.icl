implementation module Chapter6

// Examples showing distributed tasks

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows6 world

flows6 :: [Workflow]
flows6 =  [w1, w2]

w1 = workflow "CEFP/Chap 6/1. Delegate a task" 	"Delegate a task to some administrated user" 	(delegate someTask)
w2 = workflow "CEFP/Chap 6/2. Appointment" 		"Try to make an appointment with several user" 	mkAppointment

// Delegating a task to someone

delegate :: (Task a) -> Task a | iTask a
delegate task
    =                	get users
      >>= 				enterChoice "Select a user"
      >>= \user   -> 	user @: task
      >>= \result -> 	updateInformation "Check result" result

someTask :: Task Note
someTask = enterInformation "Enter Information"

// Ask everyone if they can meet on acertain time and date

derive class iTask Enquire, YesNoBecause

:: Enquire	=	{ date 		:: Display Date
				, time 		:: Display Time
				, canJoin 	:: YesNoBecause
				}
:: YesNoBecause = Yes | No Note

mkAppointment ::  Task [Enquire]
mkAppointment 
    =                	enterInformation "Which date do you want to meet ?"
      >>= \date ->		enterInformation "Which time do you want to meet ?"
      >>= \time ->		get users
      >>= \users ->		enterMultipleChoice "Which users need to join the meeting ?" users
      >>= \selected  -> mapTask  (\u -> u @: updateInformation "Can we meet ?" {date=Display date, time= Display time, canJoin=Yes}) selected
      >>= \answers ->	showMessageAbout "Users answered" answers
      
mapTask :: (a -> Task b) [a] -> Task [b] | iTask b
mapTask f []  			
	= return []
mapTask f [a:as]  
	=				f a
		>>= \b ->	mapTask f as
		>>= \bs ->  return [b:bs]
