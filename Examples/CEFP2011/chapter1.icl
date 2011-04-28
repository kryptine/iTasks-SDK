module chapter1

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine 
				[ w1, w2, w3, w4, w5, w6a, w6b, w6c, w7, w8, w9
				] world

// a simple form for an integer value

w1 = workflow "CEFP/1: Int" "Fill in an integer value" taskIntForm

taskIntForm :: Task Int
taskIntForm = fillInForm "Please fill in an integer value:"

fillInForm :: String -> Task a | iTask a
fillInForm prompt
	= 					enterInformation prompt
		>>=	\result ->	showMessageAbout "The result is:" result

// same, just to show eta conversion

taskIntForm2 :: Task Int
taskIntForm2 = fillInForm2 "Please fill in an integer value:"

fillInForm2 :: String -> Task a | iTask a
fillInForm2 prompt
	= 					enterInformation prompt
		>>=				showMessageAbout "The result is:" 

// show the power of generic functions: a form for type person

w2 = workflow "CEFP/2: Person" "Fill in person record" personForm

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

// same, now for [Person]

w3 = workflow "CEFP/3: [Person]" "Fill in list of persons" personAdmForm

personAdmForm :: Task [Person]
personAdmForm = fillInForm "Please fill in the form:"

// example of a recursive definition

w4 = workflow "CEFP/4: [Person] Check" "Fill in and check list of persons" 	fillInAndCheckPersons

fillInAndCheckPersons :: Task [Person]
fillInAndCheckPersons =  fillInAndCheck "Please fill in the form:"

fillInAndCheck ::  String -> Task a | iTask a
fillInAndCheck prompt
	=					enterInformation prompt
		>>= \result ->	repeatUntilOK (updateInformation prompt) result

repeatUntilOK :: (a -> Task a) a -> (Task a) | iTask a
repeatUntilOK task result
		=				requestConfirmationAbout "Is the result ok ?" result
			>>= \ok ->	if ok (return result)
							  (task result >>= repeatUntilOK task) 

// delegate: example of a higher order task 

w5 = workflow "CEFP/5: Delegate" "Delegate CEFP/4" (delegate fillInAndCheckPersons)

selectUser
		= 					getUsers
			>>=				enterChoice "Select a user:"

delegate :: (Task a) -> (Task a) | iTask a
delegate task
	= 						selectUser
		>>= \worker ->		worker @: task
		>>= \result ->		updateInformation "Check result" result
		
// would be nice to play with workflows as tasks... not yet implemented

/* this needs more work...
delegateWorkflow :: (Task a) | iTask a
delegateWorkflow
	=						getMyWorkflows
		>>= 				enterChoice "Select the workflow to delegate"
		>>= \wfl ->			delegate wfl
*/		
		
// example, chat using a view on shared state and explicit recursion

w6a = workflow "CEFP/6a: Chat" "Chat with one iTask user" chat1

chat1 
    =               		getCurrentUser
    	>>= \me ->			selectUser
		>>= \you ->			createSharedStore initChatState
        >>= \chatState -> 	(me  @: chatEditor me you chatState)
                    		-||-
                    		(you @: chatEditor you me chatState)
where
	chatEditor me you chatState
		= 							showMessageSharedA ("Chat list view") id [] chatState
									||-
									enterInformationA ("Chat with " <+++ you) id [(ActionQuit,always),(ActionOk,ifvalid)]
		>>= \(event,response) ->		case event of
		 								 ActionQuit -> stop
		 								 ActionOk 	-> 		updateShared (\list -> list ++ [me +++> ": " +++> fromJust response]) chatState
		 												>>|	chatEditor me you chatState

initChatState :: [String]
initChatState = []

// example, chat using modification of shared state

w6b = workflow "CEFP/6b: Chat" "Chat with one iTask user, updating views" chat2

chat2 
    =               		getCurrentUser
    	>>= \me ->			selectUser
		>>= \you ->			createSharedStore initChatState
        >>= \chatState -> 	(me @: chatEditor me you chatState)
                    		-||-
                    		(you @: chatEditor you me chatState)
where
	chatEditor me you chatState
		= 	updateSharedInformationA ("Chat with " <+++ you) (view me) actions chatState

	view user 
		=	( \list -> (Display list,Note "")
			, \(Display _,Note response) list -> list ++ [user +++> ": " +++> response]
			)

	actions = [(ActionQuit,always)]

// example, chat using shared state

w6c = workflow "CEFP/6c: Chat" "Chat with several users" chat3

chat3
    =               		getCurrentUser
    	>>= \me ->			createSharedStore initChatState
        >>= \chatState -> 	parallel "Chat application" chatState finishPar [initControl chatState] [initChat me chatState] 
where

	finishPar _ _ = Void

	initChat user chatState
		=	InBodyTask (newChat user chatState) finishChat
		
	initControl chatState
		=	InBodyCTask (appendChatter chatState)

	appendChatter chatState _
		=					selectUser
			>>= \you ->		return [AppendTasks [initChat you chatState]]		

	finishChat _ list 
		=	(list,[])

	newChat user chatState
		= 	user @: updateSharedInformationA ("Chat with iTask users") (view user) actions chatState

	view user 
		=	( \list -> (Display list,Note "")
			, \(Display _,Note response) list -> list ++ [user +++> ": " +++> response]
			)

	actions =  	[ (ActionQuit,always)]
			 	

// a simple button only valid when some predicates hold

w7 = workflow "CEFP/7: Accept only an odd number" "Type in an odd positive number less than 100" getOddNumber

getOddNumber :: Task Int
getOddNumber 
	=						enterInformationA "Type in an odd number" id [(ActionOk,predicate)]
		>>= \(_,value) ->	showMessageAbout "You typed in:" (fromJust value)
where
	predicate (Valid n) = n > 0 && isOdd n && n < 100
	predicate _ = False	

// guarantee that a type has values with a certain property specializing gVerify

w8 = workflow "CEFP/8: Specialized type only accepting an odd number" "Type in an odd number" getOddNumber2

:: Odd = Odd Int

derive gVisualize 	Odd
derive gUpdate 		Odd
derive gDefaultMask Odd
derive JSONEncode 	Odd
derive JSONDecode 	Odd
derive gEq 			Odd

gVerify{|Odd|} mba st
	= wrapperVerify (Just "Type in an odd number") (\(Odd v) -> isOdd v) (\(Odd v) -> v +++> " is not an odd number") mba st

getOddNumber2 :: Task Int
getOddNumber2 
	=						enterInformation "Type in an odd number" 
		>>= \(Odd n) ->		showMessageAbout "You typed in:" n

// pocket calculator

// making an appointment

w9 = workflow "CEFP/9: Arrange a meeting date between several users" "Arrange meeting" mkAppointment

:: MeetingProposal 
	=	{ date 		:: Date
		, time		:: Time
		, canMeet	:: [Participant]
		}
:: Participant
	=	{ name		:: User
		, canAttend :: Bool
		, comment	:: Maybe Note
		}	

derive class iTask MeetingProposal, Participant

mkAppointment :: Task Void
mkAppointment
	=					getUsers
		>>= \all ->		enterMultipleChoice "Who should attend the meeting ?" all
		>>= \users ->	enterInformation "Propose meeting dates"
		>>= \dates ->	mapAll users dates
where
	mapAll users dates  
		=						createSharedStore initMeetingState
        >>= \meetingState -> 	parallel "Meeting Date Flow" meetingState finishPar [] (map (initMeeting meetingState) users)
	where
		initMeetingState :: [MeetingProposal]
		initMeetingState =  [ { date 	= date
							  , time 	= time
							  , canMeet = [ { name 		= user
							  			    , canAttend = False
							  			    , comment 	= Nothing
							  			    } 
							  			  \\ user <- users
							  			  ]
							  }
							\\ (date,time) <- dates
							]

	finishPar _ _ = Void

	fun _ state 
		=	(state,[])

	initMeeting meetingState user
		= DetachedTask	managerProperties actionMenu meetingTask fun
	where
		meetingTask
			= updateSharedInformationA "When can we meet ?" (toView,fromView) [] meetingState

		managerProperties
			= { worker = user, priority	= NormalPriority, deadline = Nothing, status = Active }	
		
		actionMenu actions = []
		
		toView list = Table list
		
		fromView (Table mlist) list = mlist		
