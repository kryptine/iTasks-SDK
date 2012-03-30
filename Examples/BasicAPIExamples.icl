implementation module BasicAPIExamples
import iTasks, UserAdmin, WorkflowAdmin
import Text
/**
* This module contains a series of small examples of basic usage of the iTasks API.
*/

//* utility functions


hasValue (Value _ _) = True
hasValue _ = False

getValue (Value v _) = v

returnF :: (a -> b) (TaskValue a) -> Task b | iTask b
returnF fun (Value v _) = return (fun v)

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v 

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
viewCurDateTime = viewSharedInformation "The current date and time is:" [] currentDateTime

personStore :: Shared [MyPerson]
personStore = sharedStore "Persons" []

editStoredPersons :: Task [MyPerson]
editStoredPersons = updateSharedInformation "Update the stored list of persons" [] personStore

viewStoredPersons :: Task [MyPerson] 
viewStoredPersons = viewSharedInformation "These are the currently stored persons" [] personStore


//* Sequential task composition

palindrome :: Task (Maybe String)
palindrome 
	=   	enterInformation "Enter a palindrome" []
		>>* [ OnAction  ActionOk      ifPalindrome  (returnF Just)
            , OnAction  ActionCancel (const True)   (returnC Nothing)
            ]
where
	ifPalindrome (Value v _) = l == reverse l
	where l :: [Char]
		  l = fromString v
	ifPalindrome _ = False

//* Parallel task composition

derive class iTask Statistics

:: Statistics = {lineCount :: Int
				,wordCount :: Int
				}

edit :: Task Note
edit = withShared (Note "") edit`
where
	edit` note
		=	viewSharedInformation "Statistics:" [ViewWith stat] note 
			-||-
			updateSharedInformation "Edit text:" [] note
			
stat (Note text) = {lineCount = length lines , wordCount = length words}
where
	lines = split "\n" text
	words = split " " (replaceSubString "\n" " " text)

horizontal = AfterLayout (tweakTUI (setDirection Horizontal))

//* Distributing tasks

delegate :: (Task a) -> Task a | iTask a
delegate task
	=							enterSharedChoice "Select someone to delegate the task to:" [] users
		>>= \user -> user @: 	(task >>= return)
		>>= \result ->			viewInformation "The result is:" [] result


// plan meeting

testMeeting :: Task DateTime
testMeeting
	=	enterSharedMultipleChoice ("Choose users","Select the users with whom you want to plan a meeting") [] users
	>>=	planMeeting
	
planMeeting :: [User] -> Task DateTime
planMeeting users =   enterDateTimeOptions
                  >>* [askPreferences users]
                  >>* [tryAgain users,decide]

enterDateTimeOptions :: Task [DateTime]
enterDateTimeOptions = enterInformation "Enter options" []

askPreferences :: [User] -> TaskStep [DateTime] [(User,[DateTime])]
askPreferences users
  = OnAction (Action "Continue") hasValue (ask users o getValue)

ask :: [User] [DateTime] -> Task [(User,[DateTime])]
ask users options
	= parallel "Collect possibilities"
	  [ (Embedded, monitor) 
	  :[(Detached (worker u),select u options) \\ u <- users]
	  ]
	  @ \answers -> [a \\ (_,Value a _) <- answers]

monitor :: ParallelTask a | iTask a
monitor = \all_results -> viewSharedInformation "Results so far" [] (mapRead tl (taskListState all_results)) @? \_ -> NoValue

select :: User [DateTime] -> ParallelTask (User,[DateTime])
select user options = \_ -> (enterMultipleChoice "Enter preferences" [] options @ \choice -> (user,choice))
 
tryAgain :: [User] -> TaskStep [(User,[DateTime])] DateTime
tryAgain users
  = OnAction (Action "Try again") (const True) (const (planMeeting users))
 
decide :: TaskStep [(User,[DateTime])] DateTime
decide = OnAction (Action "Make decision") hasValue (pick o getValue)

pick :: [(User,[DateTime])] -> Task DateTime
pick user_dates
  =   (enterChoice "Choose date" [] (transpose user_dates) @ fst)
      -||-
      (enterInformation "Enter override" [])
  >>* [OnAction (Action "Continue") hasValue (return o getValue)]

transpose :: [(a,[b])] -> [(b,[a])] | Eq b
transpose a_bs = [(b,[a \\ (a,bs) <- a_bs | isMember b bs]) \\ b <- removeDup (flatten (map snd a_bs))]

worker :: User -> ManagementMeta
worker (AuthenticatedUser id _ _) = {noMeta & worker = UserWithId id}

//* Customizing interaction with views

//* Layout tuning


//* Running the tasks in a workflow browser

bae 		:== "Basic API Examples"
basicTypes	:== bae +++ "/Interaction with basic types/"
costumTypes :== bae +++ "/Interaction with custom types/"
sharedData	:== bae +++ "/Interaction with shared data/"
seqTasks	:== bae +++ "/Sequential task composition/"
parTasks	:== bae +++ "/Parallel task composition/"
distrTask	:== bae +++ "/Distributed tasks/"

basicAPIExamples :: [Workflow]
basicAPIExamples =
	[workflow (basicTypes +++ "Hello world") 			 	"View a constant string" 			helloWorld
	,workflow (basicTypes +++ "Enter a string") 		 	"Entering a string" 				enterString
	,workflow (basicTypes +++ "Enter an integer") 		 	"Entering an integer" 				enterInt
	,workflow (basicTypes +++ "Enter a date & time") 	 	"Entering a date & time" 			enterDateTime

	,workflow (costumTypes +++ "Enter a person") 		 	"Entering a person" 				enterPerson
	,workflow (costumTypes +++ "Enter multiple persons") 	"Entering multiple persons" 		enterPersons

	,workflow (sharedData +++ "View date and time")		 	"View the current date and time" 	viewCurDateTime
	,workflow (sharedData +++ "Edit stored persons") 	 	"Update a stored list of persons" 	editStoredPersons
	,workflow (sharedData +++ "View stored persons") 	 	"View a stored list of persons" 	viewStoredPersons

	,workflow (seqTasks +++ "Palindrome") 	 			 	"Enter a Palindrome" 				palindrome

	,workflow (parTasks +++ "Simple editor with statistics")"Edit text" 						edit

	,workflow (distrTask +++ "Delegate Enter a person")    	"Delegate Enter a person" 			(delegate enterPerson)
	,workflow (distrTask +++ "Plan meeting") 				"Plan meeting" 						testMeeting

	,workflow "Manage users" 							 	"Manage system users..." 			manageUsers
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

