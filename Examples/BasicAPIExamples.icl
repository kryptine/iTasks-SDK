implementation module BasicAPIExamples
import iTasks, UserAdmin, WorkflowAdmin

/**
* This module contains a series of small examples of basic usage of the iTasks API.
*/

//* Basic interaction

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

//Generate boiler-plate code for user-defined types
derive class iTask MyPerson, MyGender

helloWorld :: Task String
helloWorld = viewInformation "You have a message from iTasks:" [] "Hello world!" 

enterPerson :: Task MyPerson 
enterPerson = enterInformation "Enter your personal information" []

enterPersons :: Task [MyPerson]
enterPersons = enterInformation "Enter personal information of multiple people" []

//* Interaction with shared data

viewCurDateTime :: Task DateTime
viewCurDateTime = viewSharedInformation "The current date and time are:" [] currentDateTime

personStore :: Shared [MyPerson]
personStore = sharedStore "Persons" []

editStoredPersons :: Task [MyPerson]
editStoredPersons = updateSharedInformation "Update the stored list of persons" [] personStore

viewStoredPersons :: Task [MyPerson] 
viewStoredPersons = viewSharedInformation "These are the currently stored persons" [] personStore

//* Sequential task composition

//* Parallel task composition

//* Distributing tasks

//* Customizing interaction with views

//* Layout tuning


//* Running the tasks in a workflow browser
basicAPIExamples :: [Workflow]
basicAPIExamples =
	[workflow "Basic API Examples/Interaction with basic types/Hello world" "View a constant string" helloWorld
	,workflow "Basic API Examples/Interaction with basic types/Enter a string" "Entering a string" enterString
	,workflow "Basic API Examples/Interaction with basic types/Enter an integer" "Entering an integer" enterInt
	,workflow "Basic API Examples/Interaction with basic types/Enter a date & time" "Entering a date & time" enterDateTime
	,workflow "Basic API Examples/Interaction with custom types/Enter a person" "Entering a person" enterPerson
	,workflow "Basic API Examples/Interaction with custom types/Enter multiple persons" "Entering multiple persons" enterPersons
	,workflow "Basic API Examples/Interaction with shared data/View date and time" "View the current date and time" viewCurDateTime
	,workflow "Basic API Examples/Interaction with shared data/Edit stored persons" "Update a stored list of persons" editStoredPersons
	,workflow "Basic API Examples/Interaction with shared data/View stored persons" "View a stored list of persons" viewStoredPersons
	,workflow "Manage users" "Manage system users..." manageUsers
	]
	
Start :: *World -> *World
Start world = startEngine (browseExamples basicAPIExamples) world
where
	browseExamples examples = forever (
		 	(viewTitle "iTasks Example Collection"
		||-
		 	enterInformation ("Login","Enter your credentials and login or press continue to remain anonymous") [])
		>>* [WithResult (Action "Login") (const True) (browseAuthenticated examples)
			,Always (Action "Continue") (browseAnonymous examples)
			])
	
	browseAuthenticated examples {Credentials|username,password}
		= authenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorklist examples)
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void
	
	browseAnonymous examples
		= manageWorklist examples

