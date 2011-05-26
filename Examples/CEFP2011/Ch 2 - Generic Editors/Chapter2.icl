implementation module Chapter2

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows2 world

flows2 :: [Workflow]
flows2 =  [w1, w2, w3, w4, w5, w6, w7]

w1 = workflow "CEFP/Chap 2/1. Enter Int value"    	"Form for an integer value" 	(show taskInt)
w2 = workflow "CEFP/Chap 2/2. Enter String value" 	"Form for entering a string"	(show taskString)
w3 = workflow "CEFP/Chap 2/3. Enter Person data"  	"Form for entering person data" (show taskPerson)
w4 = workflow "CEFP/Chap 2/4. Enter [Person]" 		"Fill in person data" 			(show taskPersonList)
w5 = workflow "CEFP/Chap 2/5. Show message example" "Show message" 					(show showList)
w6 = workflow "CEFP/Chap 2/6. Choose one item" 		"Choice of one" 				(show choiceExample)
w7 = workflow "CEFP/Chap 2/7. Choose several items" "Multiple choice" 				(show mchoiceExample)

show task = task >>= showMessageAbout "The result is:"

// a simple form to type in an integer value

taskInt :: Task Int
taskInt = enterInformation "Please enter a value"

// a simple form to type in a String

taskString :: Task String
taskString = enterInformation "Please enter a value"

// a simple form to type in a value of type Person

:: Person 	= 	{ firstName    	:: String
		      	, surName  		:: String
		      	, dateOfBirth 	:: Date
		      	, gender	 	:: Gender
		      	}
:: Gender 	=	Male
			|	Female
derive class iTask Person, Gender

taskPerson :: Task Person
taskPerson = enterInformation "Please enter a value"

// idem, now for [Person]

taskPersonList :: Task [Person]
taskPersonList = enterInformation "Please enter a value"

// idem, now for [Person]

showList :: Task (String,[Int])
showList = showMessageAbout "Look at his value" ("Here follows a list",[1..10])

//

choiceExample :: Task Int
choiceExample = enterChoice "Choose a number" [0..9]

//

mchoiceExample :: Task [Char]
mchoiceExample = enterMultipleChoice "Choose a number" ['a'..'z']
