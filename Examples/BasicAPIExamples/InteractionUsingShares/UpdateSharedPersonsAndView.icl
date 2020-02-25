implementation module BasicAPIExamples.InteractionUsingShares.UpdateSharedPersonsAndView

// Update a shared list of persons while viewing its content

import iTasks

wf :: String -> Workflow
wf a = workflow a "Edit a shared list of persons" enterSharedPersons

main :: Task ()
main = enterSharedPersons @! ()

:: Person =
	{ name			:: String
	, gender		:: Gender
	, dateOfBirth	:: Date
	}

:: Gender = Male | Female

derive class iTask Person, Gender

enterSharedPersons :: Task [Person]
enterSharedPersons
	= 				withShared []																		// create an empty shared list
	\sharedList ->  (Hint "Modify the Shared List of Persons:" @>> updateSharedInformation [] sharedList)			// update that list
					-||
					(Hint "Current Content of this Shared List:" @>> viewSharedInformation [] sharedList) 	 		// while showing that list
						<<@ ArrangeHorizontal															// show both list next to each other (default is below)
	>>! \result ->  Hint "The List contains the following:" @>> viewInformation [] result								// show the final result
	>>!				return																				// done
