module chapter1

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine 
				[ workflow "Summerschool/1: Int" 				"Fill in an integer value"  			taskIntForm
				, workflow "Summerschool/2: Person"   			"Fill in person record" 				personForm
				, workflow "Summerschool/3: [Person]"  			"Fill in list of persons" 				personAdmForm
				, workflow "Summerschool/4: [Person] Check"   	"Fill in and check list of persons" 	personAdmForm
				, workflow "Summerschool/5: Delegate "   		"Delegate example 4" 					(delegate fillInAndCheckPersons)
				] world

			

// example, simple form for integer

taskIntForm :: Task Int
taskIntForm = fillInForm "Please fill in an integer value:"

fillInForm :: String -> Task a | iTask a
fillInForm prompt
	= 					enterInformation prompt
		>>=	\result ->	showMessageAbout "The result is:" result

// example, showing eta conversion

taskIntForm2 :: Task Int
taskIntForm2 = fillInForm2 "Please fill in an integer value:"

fillInForm2 :: String -> Task a | iTask a
fillInForm2 prompt
	= 					enterInformation prompt
		>>=				showMessageAbout "The result is:" 

// example, a form for any type

:: Person 	= 	{ firstName    	:: String
		      	, surName  		:: String
		      	, dateOfBirth 	:: Date
		      	, gender	 	:: Gender
		      	}
:: Gender 	=	Male
			|	Female

derive class iTask Person, Gender

personForm :: Task Person
personForm = fillInForm "Please fill in the form:"

// example, a form for any type

personAdmForm :: Task [Person]
personAdmForm = fillInForm "Please fill in the form:"

// example, recursion

fillInAndCheckPersons :: Task [Person]
fillInAndCheckPersons =  fillInAndCheck "Please fill in the form:"

fillInAndCheck ::  String -> Task a | iTask a
fillInAndCheck prompt
	=				enterInformation prompt
		>>=			checkInformation
where
	checkInformation result
		=				requestConfirmationAbout "Is the result ok ?" result
			>>= \ok ->	if ok (return result)
							  (updateInformation prompt result >>= checkInformation) 

// example, delegate

selectUsers
		= 					getUsers
			>>=				enterChoice "Select a user:"

delegate :: (Task a) -> (Task a) | iTask a
delegate task
	= 						selectUsers
		>>= \worker ->		worker @: task
		>>= \result ->		updateInformation "Check result" result
		
// example, delegate one of my workflows


//from WorkflowStarter import getMyWorkflows	// clearly NOT the place where does should have been defined....

/* this needs more work...
delegateWorkflow :: (Task a) | iTask a
delegateWorkflow
	=						getMyWorkflows
		>>= 				enteChoice "Select the workflow to delegate"
		
*/		
		





