module ClientExecution

import RunOnClient

bae 		:== "Basic API Examples"
basicTypes	:== bae +++ "/Interaction with basic types/"
costumTypes :== bae +++ "/Interaction with custom types/"

basicAPIExamples :: [Workflow]
basicAPIExamples =
	[workflow (basicTypes +++ "Hello world") 			 	"View a constant string" 			(runOnClient helloWorld)
	,workflow (basicTypes +++ "Enter a string") 		 	"Entering a string" 				(runOnClient enterString)
	,workflow (basicTypes +++ "Enter an integer") 		 	"Entering an integer" 				(runOnClient enterInt)
	,workflow (basicTypes +++ "Enter a date & time") 	 	"Entering a date & time" 			(runOnClient enterDateTime)

	,workflow (costumTypes +++ "Enter a person") 		 	"Entering a person" 				(runOnClient enterPerson)
	]
	
//* Basic interaction

helloWorld :: Task String
helloWorld = viewInformation "You have a message from iTasks:" [] "Hello world!" 

enterString :: Task String
enterString = enterInformation "Enter a string" []

enterInt :: Task Int
enterInt = enterInformation "Enter an integer" []

enterDateTime :: Task DateTime
enterDateTime = enterInformation "Enter a date and time" []

viewIntList :: Task [Int]
viewIntList = viewInformation "View the numbers from 1 to 10" [] [1..10]

//* Interaction using user-defined types

:: MyPerson =
	{ name			:: String
	, gender		:: MyGender
	, dateOfBirth	:: Maybe Date
	}
	
:: MyGender = Male | Female

derive class iTask MyPerson, MyGender


enterPerson :: Task MyPerson 
enterPerson = enterInformation "Enter your personal information" []
    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist basicAPIExamples)) world
 