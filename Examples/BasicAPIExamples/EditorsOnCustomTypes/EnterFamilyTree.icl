implementation module BasicAPIExamples.EditorsOnCustomTypes.EnterFamilyTree

// Enter a family tree using a record type

import iTasks
import iTasks.Extensions.DateTime

wf :: String -> Workflow
wf a = workflow a "Enter a family tree" enterFamily

main :: Task ()
main = enterFamily @! ()

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

enterFamily :: Task Family
enterFamily
	=   Hint "Enter a family tree:" @>> enterInformation []
	>>! \result -> Hint "You Entered:" @>> viewInformation [] result
