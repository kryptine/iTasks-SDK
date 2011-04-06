module chapter1

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine 
				[ workflow "Summerschool/Int" 				"Fill in an integer value"  			taskIntForm
				, workflow "Summerschool/Person"   			"Fill in person record" 				personForm
				, workflow "Summerschool/[Person]"  		"Fill in list of persons" 				personAdmForm
				, workflow "Summerschool/[Person] Check"   	"Fill in and check list of persons" 	personAdmForm
//				, workflow "Demo Example Delegate" "Delegate filling in form Person" (delegate person)
				] world

			

// example 1

taskIntForm :: Task Int
taskIntForm = fillInForm "Please fill in an integer value:"

fillInForm :: String -> Task a | iTask a
fillInForm prompt
	= 					enterInformation prompt
		>>=	\result ->	showMessage "The result is:" result

taskIntForm2 :: Task Int
taskIntForm2 = fillInForm2 "Please fill in an integer value:"

fillInForm2 :: String -> Task a | iTask a
fillInForm2 prompt
	= 					enterInformation prompt
		>>=				showMessage "The result is:" 

// example

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

// example

personAdmForm :: Task [Person]
personAdmForm = fillInForm "Please fill in the form:"

// example

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



selectUsers
		= 					getUsers
			>>=				enterChoice "Select a user:"

delegate :: (Task a) -> (Task a) | iTask a
delegate task
	= 						selectUsers
		>>= \worker ->		worker @: task
		>>= \result ->		updateInformation "Check result" result
		
		 