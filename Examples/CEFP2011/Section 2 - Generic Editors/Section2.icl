implementation module Section2

import iTasks
from Section3 import show

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows2 world

flows2 :: [Workflow]
flows2 
	=   [ workflow "CEFP/Sect 2/1. Hello world!"        	"The infamous hello world"      hello
		, workflow "CEFP/Sect 2/2. Enter Int value"    		"Form for an integer value" 	(show taskInt)
		, workflow "CEFP/Sect 2/3. Enter String value" 		"Form for entering a string"	(show taskString)
		, workflow "CEFP/Sect 2/4. Enter Person data"  		"Form for entering person data" (show taskPerson)
		, workflow "CEFP/Sect 2/5. Enter [Person]" 			"Fill in person data" 			(show taskPersonList)
		, workflow "CEFP/Sect 2/6. Choose one item" 		"Choice of one" 				(show chooseNumber)
		, workflow "CEFP/Sect 2/7. Choose several items"	"Multiple choice" 				(show pizzaWith)
		, workflow "CEFP/Sect 2/8. Simple Editor" 			"Using type Note" 				(show simpleEditor)
		, workflow "CEFP/Sect 2/9. Pick a Date" 			"Using type Date" 				(show chooseDate)
		, workflow "CEFP/Sect 2/10. Point on a map" 		"Using type GoogleMap" 			(show pointOnMap)
		]

// the ubiquitous hello world example

hello = updateInformation "Press Ok to terminate" [] "Hello world!"

// a simple form to type in an integer value

taskInt :: Task Int
taskInt = enterInformation "Please enter a value" []

// a simple form to type in a String

taskString :: Task String
taskString = enterInformation "Please enter a value" []

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
taskPerson = enterInformation "Please enter a value" []

// idem, now for [Person]

taskPersonList :: Task [Person]
taskPersonList = enterInformation "Please enter a value" []

//
chooseNumber :: Task Int
chooseNumber = enterChoice "Choose a number" [] [0..9]

//
pizzaWith :: Task [String]
pizzaWith = enterMultipleChoice "What do you like on your pizza ?" [] ["Cheese","Tomato","Ansjofish","Salami"]

//
simpleEditor :: Task Note
simpleEditor = enterInformation "Enter a piece of text" []

//
chooseDate :: Task Date
chooseDate = enterInformation "Choose a date" []

//
import GoogleMaps

pointOnMap :: Task GoogleMap
pointOnMap = enterInformation "Show me the location" []

