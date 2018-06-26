implementation module BasicAPIExamples.EditorsOnCustomTypes.EnterFamilyTree

// Enter a family tree using a record type

import iTasks
import iTasks.Extensions.DateTime

wf :: String -> Workflow
wf a = workflow a "Enter a family tree" enterFamily

:: Family	=	{ person 		:: Person
				, married		:: Maybe Person
				, children		:: [Family]
				}
:: Person	=	{ firstName		:: String
				, surName		:: String
				, gender		:: Gender
				, dateOfBirth	:: Date
				}
:: Gender	=	Male
			|	Female

derive class iTask Family, Person, Gender

Start :: *World -> *World
Start world
	= startEngine enterFamily world

enterFamily :: Task Family
enterFamily
	=   enterInformation "Enter a family tree:" []
	>>= viewInformation "You Entered:" []
