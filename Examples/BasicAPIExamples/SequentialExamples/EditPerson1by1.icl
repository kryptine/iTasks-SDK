implementation module BasicAPIExamples.SequentialExamples.EditPerson1by1

// Enter a person 1 by 1 to a list

import iTasks

wf :: String -> Workflow
wf a = workflow a "Edit a list of Persons one by one" (person1by1 [])

main :: Task ()
main = person1by1 [] @! ()

:: Person =
	{ name			:: String
	, gender		:: Gender
	, dateOfBirth	:: Date
	}

:: Gender = Male | Female

derive class iTask Person, Gender

person1by1 :: [Person] -> Task [Person]
person1by1 persons
	=       enterInformation [EnterWithHint "Add a person"]
			-||
			viewInformation [ViewWithHint "List so far.."] persons
	>>*		[ OnAction  (Action "Add") 		(hasValue (\v -> person1by1  [v : persons]))
		    , OnAction  (Action "Finish")   (always (return persons))
		    , OnAction  ActionCancel 		(always (return []))
	        ]
