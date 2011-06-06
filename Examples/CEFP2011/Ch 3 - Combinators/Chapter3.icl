implementation module Chapter3

// Examples showing the usage of frequently used iTask combinators

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows3 world

flows3 :: [Workflow]
flows3 =  [w1, w2, w3, w4, w5, w6, w7, w8]

w1 = workflow "CEFP/Chap 3/1. Hello"    					"\"Hello World\" in iTask" 				hello
w2 = workflow "CEFP/Chap 3/2. Form for [Person]" 			"Form for [Person]"						(show personList)
w3 = workflow "CEFP/Chap 3/3. Choose one from [Person]"  	"Choose one from a list" 				(show personList2)
w4 = workflow "CEFP/Chap 3/4. Choose one or more [Person]" 	"Select from a list" 					(show personList3)
w5 = workflow "CEFP/Chap 3/5. Form for [Person]" 			"Form for [Person]" 					(show personList4)
w6 = workflow "CEFP/Chap 3/6. Tea or coffee" 				"Simple choice..." 						(show teaOrCoffee)
w7 = workflow "CEFP/Chap 3/7. Form for [Person]+check" 		"Form for [Person] and check result" 	(show fillInAndCheckPersons)
w8 = workflow "CEFP/Chap 3/8. Form for [Person]+check" 		"Form for [Person] and check result" 	(show fillInAndCheckPersons2)

show :: (Task a) -> Task a | iTask a
show task = task >>= \result -> showInformation "The result is:" [] result


// Hello World

hello :: Task String
hello 
	=              		enterInformation "Please enter your name" []
        >>= \name -> 	showInformation ("Hello " +++ name +++ "!") [] name
        
// Typing in a [Person], showing the result

:: Person 	= 	{ firstName    	:: String
		      	, surName  		:: String
		      	, dateOfBirth 	:: Date
		      	, gender	 	:: Gender
		      	}
:: Gender 	=	Male
			|	Female
derive class iTask Person, Gender

personList :: Task [Person]
personList
	=              		enterInformation "Please fill in the form" []
		
// Same as w32, showing eta conversion

personList2 :: Task Person
personList2
	=              		personList
		>>= 			enterChoice "Choose one: " []
		
// Same as w32, showing higher order functions

personList3 :: Task [Person]
personList3
	=              		personList
		>>=				enterMultipleChoice "Select one or more: " []

// Same as w32, showing recursive variant

personList4 :: Task [Person]
personList4
	=          			fillOne
		>>= \person ->  enterChoice "One more ? "  [] ["Yes","No"]
		>>= \answer ->	case answer of
							"Yes" -> 					personList4 
										>>= \persons ->	return [person:persons]
							"No" ->						return [person]
where
	fillOne :: Task Person
	fillOne = enterInformation "Please fill in the form" []

// Simple choice

teaOrCoffee = enterChoice "Choose an option" [] ["Tea","Coffee"]

// Check result afterwards, not happy: do it again

fillInAndCheckPersons :: Task [Person]
fillInAndCheckPersons =  repeatUntilApproved (enterInformation "Please fill in the form:" [])

repeatUntilApproved :: (Task a) -> Task a | iTask a
repeatUntilApproved task
    =            task
      >>= \v  -> enterChoice "Approve result: "  [About v] ["Yes","No"]
      >>= \ok -> case ok of
                    "Yes" -> return v
                    "No"  -> repeatUntilApproved task
                    
fillInAndCheckPersons2 :: Task [Person]
fillInAndCheckPersons2 =  repeatUntilApproved2 (enterInformation "Please fill in the form:" [])

:: Approve = Yes | No

derive class iTask Approve
repeatUntilApproved2 :: (Task a) -> Task a | iTask a
repeatUntilApproved2 task
    =            task
      >>= \v  -> enterChoice "Approve result: "  [About v] [Yes,No]
      >>= \ok -> case ok of
                    Yes -> return v
                    No  -> repeatUntilApproved2 task

positive :: Task Int
positive = while ((>=) 0) (updateInformation "Please enter a positive number" []) 0

while :: (a -> Bool) (a -> Task a) a -> Task a | iTask a
while cond task v
| cond v	= task v >>= while cond task
| otherwise	= return v
