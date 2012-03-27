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
 
//* Interaction using user-defined types

//* Interaction with shared data

//* Sequential task composition

//* Parallel task composition

//* Distributing tasks

//* Customizing interaction with views

//* Layout tuning


//* Running the tasks in a workflow browser
basicAPIExamples :: [Workflow]
basicAPIExamples =
	[workflow "Basic API Examples/Basic interaction/Enter a string" "Entering a string..." enterString
	,workflow "Basic API Examples/Basic interaction/Enter an integer" "Entering an integer..." enterInt
	,workflow "Basic API Examples/Basic interaction/Enter a date & time" "Entering a date & time..." enterDateTime
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

