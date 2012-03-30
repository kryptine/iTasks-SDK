implementation module BasicAPIExamples
import iTasks, UserAdmin, WorkflowAdmin
import Text
/**
* This module contains a series of small examples of basic usage of the iTasks API.
*/

//* utility functions

always = const True

hasValue (Value _ _) = True
hasValue _ = False

getValue (Value v _) = v

returnF :: (a -> b) (TaskValue a) -> Task b | iTask b
returnF fun (Value v _) = return (fun v)

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

toMaybe :: (TaskValue a) -> Maybe a
toMaybe (Value v _) =  (Just v)
toMaybe _   =  Nothing

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

linesPar :: Task (Maybe String)
linesPar
	=	withShared "" doEditor
where
	doEditor state
		= 			noteE state 
					-||- 
					lineE state
			>>* 	[OnAction ActionQuit always (return o toMaybe)]

	noteE state 
		= 			updateSharedInformation ("Text","Edit text") [noteEditor] state
			>>*		[ OnAction (Action "Trim") always 	(\txt -> update trim state >>| noteE state)	
					]

	lineE state
		=	updateSharedInformation ("Lines","Edit lines") [listEditor] state

	noteEditor = UpdateWith (\txt -> Note txt) (\_ (Note txt) -> txt)
	listEditor = UpdateWith (split "\n") (\_ l -> join "\n" l)

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
//
calculateSum :: Task Int
calculateSum
  =   enterInformation ("Number 1","Enter a number") []
  >>= \num1 ->
      enterInformation ("Number 2","Enter another number") []
  >>= \num2 ->
      viewInformation ("Sum","The sum of those numbers is:") [] (num1 + num2)
//
calculateSumSteps :: Task Int
calculateSumSteps = step1 0 0
where
	step1 n1 n2		=		updateInformation ("Number 1","Enter first number")  [] n1
						>>*	[ OnAction ActionNext hasValue ((\n1 -> step2 n1 n2) o getValue)
							]
	step2 n1 n2		=		updateInformation ("Number 2","Enter second number") [] n2
						>>*	[ OnAction ActionPrevious always 	(const (step1 n1 n2))
							, OnAction ActionNext     hasValue ((\n2 -> step3 n1 n2) o getValue)]
	step3 n1 n2		=		viewInformation ("Sum","The sum of those numbers is:") [] (n1 + n2)
						>>*	[ OnAction ActionPrevious always 	(const (step2 n1 n2))
						  	, OnAction ActionOk  always  		(returnC (n1 + n2))
						  	]
//
:: MySum = {firstNumber :: Int, secondNumber :: Int, sum :: Display Int}
derive class iTask MySum

calculateSum2 :: Task Int
calculateSum2
  = 				updateInformation ("Sum of 2 numbers, with view","") 
  						[UpdateWith (\(i,j) -> {firstNumber = i, secondNumber = j, sum = Display (i+j)}) 
  						            (\_ res -> (res.firstNumber,res.secondNumber))] (0,0)
  	>>= \(i,j) -> 	return (i+j)

//
coffeemachine :: Task (String,EUR)
coffeemachine  
	=	enterChoice ("Product","Choose your product:") []
					[("Coffee", EUR 100)
					,("Cappucino", EUR 150)
					,("Tea", EUR 50)
					,("Chocolate", EUR 100)
					] 
	>>=  getCoins (EUR 0)
	>>|  coffeemachine

getCoins :: EUR (String,EUR) -> Task (String,EUR)
getCoins paid (product,toPay) 
	= 				viewInformation "Coffee Machine" [ViewWith view1] toPay
					||-		
					enterChoice  ("Insert coins","Please insert a coin...") [] coins
			>>*		[ OnAction ActionCancel 		always (const (stop ("Cancelled",paid)))
					, OnAction (Action "Insert") 	always (handleMoney o getValue)
					]
where				
	coins	= [EUR 5,EUR 10,EUR 20,EUR 50,EUR 100,EUR 200]

	handleMoney coin 
	| toPay > coin	= getCoins (paid+coin) (product, toPay-coin)
	| otherwise		= stop (product,coin-toPay)
	
	stop (product, money) = viewInformation "Coffee Machine" [ViewWith view2] (product,money)

	view1 toPay 		   = [(DivTag [] [Text ("Chosen product: " <+++ product), BrTag [], Text ("To pay: " <+++ toPay)])]
	view2 (product,money)  = [(DivTag [] [Text ("Chosen product: " <+++ product), BrTag [], Text ("Money returned: " <+++ money)])]

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

// delegate

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
	,workflow (sharedData +++ "Edit note or List of strings") "Edit note or List of strings" 	linesPar

	,workflow (seqTasks +++ "Palindrome") 	 			 	"Enter a Palindrome" 				palindrome
	,workflow (seqTasks +++ "Sum of two numbers") 	 		"Sum of two numbers" 				calculateSum
	,workflow (seqTasks +++ "Sum, with backstep") 	 		"Sum, with backstep" 				calculateSumSteps
	,workflow (seqTasks +++ "Sum of two numbers") 	 		"Sum of two numbers 2" 				calculateSum2
	,workflow (seqTasks +++ "Coffee Machine") 	 			"Coffee Machine" 					coffeemachine

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

