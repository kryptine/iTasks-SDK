module ProductivitySuite
/**
* This module provides a set of example workflows related to personal productivity.
* For example: Scheduling meetings and reminders, collaborating on documents etc...
*/

import iTasks
import CommonDomain

:: Message =
	{ subject	:: !String
	, to		:: !UserName
	, message	:: !Note
	}
	
derive gVisualize	Message
derive gUpdate		Message
derive gPrint		Message
derive gParse		Message

derive bimap Maybe, (,)

//Send a message
sendMessage :: Task Void
sendMessage
	=	enterInformation "Write message"
	>>=	\{Message|to,subject,message = Note body} ->
		to @: (subject, showMessage body) 

//Scheduling a meeting
scheduleMeeting :: Task (Date,Time)
scheduleMeeting		
	=					getCurrentUser -&&- chooseUser "Who do yo want to schedule a meeting with?"
	>>= \(me,you) 	->	enterInformation "When do you want to schedule the meeting?"	
	>>= \datetime	->	findDate you datetime
	>>= \datetime	->	(confirm you me datetime -&&- confirm me you datetime)
	>>|					return datetime
where
	findDate :: User (Date,Time) -> Task (Date, Time)
	findDate user datetime
		= user @: ("Meeting request", proposeMeeting datetime)
		>>= \(ok,alternative) -> if ok
			(return datetime)
			(requestConfirmationAbout ("Your proposed date (" <+++ datetime <+++ ") was not possible, is this alternative ok?") alternative
			 >>= \ok -> if ok
			 (return alternative)
			 (updateInformation "Please suggest an alternative date" datetime >>= \datetime -> findDate user datetime)
			)
	
	proposeMeeting :: (Date,Time) -> Task (Bool,(Date,Time))
	proposeMeeting datetime
		= requestConfirmationAbout "Can we meet on the following date?" datetime
		>>= \ok -> if ok
			(return (True,datetime))
			(updateInformation "Please suggest an alternative date" datetime >>= \alternative -> return (False,alternative))
	
	confirm  :: User User (Date,Time) -> Task Void 
	confirm you me (date,time)
		= 	you @:
			("Meeting confirmation", showMessage ("You have a meeting with " <+++ me.displayName <+++ " on " <+++ date <+++ " at " <+++ time))
			

Start :: *World -> *World
Start world = startEngine [workflow "Schedule meeting" scheduleMeeting,workflow "Send message" sendMessage] world