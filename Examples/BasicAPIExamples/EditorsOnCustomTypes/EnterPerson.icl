implementation module BasicAPIExamples.EditorsOnCustomTypes.EnterPerson

// Enter a family tree using a record type

import iTasks
import iTasks.Extensions.DateTime

wf :: String -> Workflow
wf a = workflow a "Enter a person" enterPerson

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
	=   enterInformation "Enter a person:" []
	>>= viewInformation "You Entered:" []
