module EnterPerson

// Enter a family tree using a record type

import iTasks
import iTasks.Extensions.DateTime

:: Person	=	{ name			:: String
				, gender		:: Gender
				, dateOfBirth	:: Date
				}
:: Gender	=	Male
			|	Female

derive class iTask Person, Gender

Start :: *World -> *World
Start world 
	= startEngine enterPerson world
	
enterPerson :: Task Person
enterPerson 
	= 	enterInformation "Enter a person:" []
	>>=	viewInformation "You Entered:" []
	>>= return
	
