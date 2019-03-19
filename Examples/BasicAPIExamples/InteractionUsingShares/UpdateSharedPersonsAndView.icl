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
	\sharedList ->  updateSharedInformation "Modify the Shared List of Persons:" [] sharedList			// update that list
					-||
					viewSharedInformation "Current Content of this Shared List:" [] sharedList 	 		// while showing that list
						<<@ ApplyLayout horizontal														// show both list next to each other (default is below)
	>>=				viewInformation "The List contains the following:" [] 								// show the final result
	>>=				return																				// done
where
	horizontal = setUIAttributes (directionAttr Horizontal)
