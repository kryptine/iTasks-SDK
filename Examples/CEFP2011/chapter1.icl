module chapter1

import iTasks, StdMisc

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
		= 					get users
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

mapMaybe ::  (a -> b) (Maybe a) -> Maybe b
mapMaybe f (Just a)  = Just (f a)
mapMaybe _ Nothing  = Nothing

noResult _ _ = Void
noActions _ = []


onlyIf :: (a -> Bool) (Maybe a) -> Maybe a
onlyIf pred (Just a)
	| pred a = Just a
onlyIf _ _ = Nothing


initChatState :: [String]
initChatState = []

chat1 
    =               		get currentUser
    	>>= \me ->			selectUser
		>>= \you ->			parallel "2 Chatters" initChatState noResult
								[DetachedTask (normalTask me)  noMenu (chatEditor me you)
								,DetachedTask (normalTask you) noMenu (chatEditor you me)
								]
where
	chatEditor me you chatState os
		= 				(monitor ("Chat list view") id (const False) False chatState)
						||-
						enterInformationA ("Chat with " <+++ you) 
						(\a ->	[ (ActionQuit, Just (return Void))
								, (ActionOk,   mapMaybe (\(Note a) ->	update (\list -> list ++ [me +++> ": " +++ a]) chatState
															>>|	chatEditor me you chatState os 
															>>| return Void) a)
								]
						) 
										
			>>= 		id

// example, chat using modification of shared state

w6b = workflow "CEFP/6b: Chat" "Chat with one iTask user, updating views" chat2

chat2 
    =   get currentUser
    	>>= \me ->			selectUser
		>>= \you ->			parallel "2 Chatters" initChatState noResult
								[DetachedTask (normalTask me)  noMenu (chatEditor me you)
								,DetachedTask (normalTask you) noMenu (chatEditor you me)
								]
where
	chatEditor me you chatState os
		= 	updateSharedInformationA ("Chat with " <+++ you) (view me) actions chatState

	view user 
		=	( \list -> (Display list,Note "")
			, \(_,Note response) list -> list ++ [user +++> ": " +++> response]
			)

	actions _ = [(ActionQuit, Just Void)]

// example, chat using shared state

w6c = workflow "CEFP/6c: Chat" "Chat with several users" chat3

normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

derive class iTask ChatState, Message

:: ChatState	=	{ chatters  :: [User]
				    , chats		:: [Message]
				    }
:: Message	=		{ chatting	:: User
					, message	:: String
					}
emptyChatState = {chatters = [], chats = []}

chat3
    =               		get currentUser
    	>>= \me ->			parallel "Chat application" emptyChatState finished [InBodyTask (chatTask me)]
where

	finished _ _ = Void

	chatTask user chatState os
		=								update addUser chatState
			>>|							chatMore ""
	where
		chatMore content =				(monitor ("Chat list view") id (const False) False chatState)
										||-
										updateInformationA ("Chat with iTask users") (toView,fromView) actions content	
			>>= \(event,response) -> 	case event of
											ActionAdd -> 			set os [AppendTask (WindowTask "Append Chatter" noMenu handleNewChatter)]
															>>|		chatMore (fromJust response)
											ActionOk ->				update (addMessage (fromJust response)) chatState 
															>>|		chatMore ""	
											ActionQuit ->			update (removeUser o addMessage "bye") chatState
															>>|		return Void	
				
		(toView, fromView) = (\c -> Note c, \(Note c) _ -> c) 

		actions r =  	[(ActionOk,  Just (ActionOk,r))
						,(ActionAdd, Just (ActionAdd,r))
						,(ActionQuit,Just (ActionQuit,r))
						]



		handleNewChatter chatState os
			=						selectUser
				>>= \someone ->		set os [AppendTask (DetachedTask (normalTask someone) noMenu (chatTask someone))]

		addMessage message 	cs = {cs & chats = cs.chats ++ [{chatting = user, message = message}]}
		addUser 			cs = {cs & chatters = [user:cs.chatters]}
		removeUser 			cs = {cs & chatters = removeMember user cs.chatters}


ActionAdd :== Action "Add Chatter" "Add Chatter"


// a simple button only valid when some predicates hold

w7 = workflow "CEFP/7: Accept only an odd number" "Type in an odd positive number less than 100" getOddNumber

getOddNumber :: Task Int
getOddNumber 
	=					enterInformationA "Type in an odd number" action 
		>>= \value ->	showMessageAbout "You typed in:" value
where
	action n	= [ (ActionOk, onlyIf (\n -> (n > 0 && isOdd n && n < 100)) n)]

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

// pocket calculator, see Steffens example...

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
:: MeetingProposalView
	=	{ date 		:: Date
		, time		:: Time
		, canMeet	:: [VisualizationHint ParticipantView]
		}
:: ParticipantView
	=	{ name		:: Display User
		, canAttend :: Bool
		, comment	:: Maybe Note
		}	
derive class iTask MeetingProposal, Participant, MeetingProposalView, ParticipantView

mkAppointment :: Task [MeetingProposal]
mkAppointment
	=					get users
		>>= \all ->		enterMultipleChoice "Who should attend the meeting ?" all
		>>= \users ->	enterInformation "Propose meeting dates"
		>>= \dates ->	let initMeetingState = [ { MeetingProposal
												 | date    = date
												 , time    = time
												 , canMeet = [ { Participant
												               | name      = user
												 			   , canAttend = False
												 			   , comment   = Nothing
												 			   } 
												 			 \\ user <- users
												 			 ]
												 }
											   \\ (date,time) <- dates
											   ]
		                 in parallel "Meeting Date Flow" initMeetingState finishPar [manage : map initMeeting users]
where
	finishPar _ s = s

	initMeeting user
		= DetachedTask managerProperties noMenu meetingTask
	where

		meetingTask :: (SymmetricShared [MeetingProposal]) (ParallelInfo [MeetingProposal]) -> Task [MeetingProposal]
		meetingTask meetingState _
			= updateSharedInformationA "When can we meet ?" (viewForUser user,modelFromView) noActions meetingState

		managerProperties
			= { worker = user, priority	= NormalPriority, deadline = Nothing, status = Active }	
		
	
	viewForUser :: User [MeetingProposal] -> [MeetingProposalView]
	viewForUser user props
		= [  {MeetingProposalView | date=date, time=time, canMeet=map (viewParticipant user) canMeet}
		  \\ {MeetingProposal | date,time,canMeet} <- props
		  ]
	
	modelFromView :: [MeetingProposalView] .a -> [MeetingProposal]
	modelFromView props _
		= [  {MeetingProposal | date=date, time=time, canMeet=map modelParticipant canMeet} 
		  \\ {MeetingProposalView | date,time,canMeet} <- props
		  ]
	
	viewParticipant :: User Participant -> VisualizationHint ParticipantView
	viewParticipant user participant=:{Participant | name, canAttend, comment}
		= if (user == name)	VHEditable VHDisplay view
	where
		view		= {ParticipantView | name = Display name
	                                   , canAttend = canAttend
	                                   , comment   = comment
	                  }
	
	modelParticipant :: (VisualizationHint ParticipantView) -> Participant
	modelParticipant view
		= { Participant | name=name, canAttend=canAttend, comment=comment }
	where
		{ParticipantView | name=Display name,canAttend,comment}	= fromVisualizationHint view
	
	manage
		= InBodyTask check
	where
		check meetingState controlState
			=     updateSharedInformationA "Monitor answers" (viewForManager,\_ ps -> ps)  actions meetingState
			  >>= \props -> enterChoice "Choose meeting" props
		where
			actions (True,r)  = [(ActionOk,Just r)]
			actions (False,r) = [(ActionOk,Nothing)]

			viewForManager :: [MeetingProposal] -> [MeetingProposal]
			viewForManager props
				= [ p \\ p=:{MeetingProposal | canMeet=can} <- props | and [canAttend \\ {Participant | canAttend} <- can] ]


/*

chat3
    =               		getCurrentUser
    	>>= \me ->			parallel "Chat application" initChatState finished [chatTask me]
where

	finished _ _ = Void

	chatTask user 
		=	DetachedTask (normalTask user) noMenu handlingTask
	where

		handlingTask chatState osState
			=		updateSharedInformationA` ("Chat with iTask users") (view user) stateActions termActions (chatState >+< osState)	
				
		where
			view user 
				=	( \(list,osinfo) -> (Display list,Note "")
					, \(Display _,Note response) (list,_) -> (list ++ [user +++> ": " +++> response],[])
					)

			termActions =  	[(ActionQuit, alwaysShared)]

			stateActions = [("Add Chatter", [AppendTask (WindowTask "Append Chatter" noMenu handleNewChatter)])] 
					handleNewChatter chatState osState
						=						selectUser
							>>= \someone ->		writeShared osState [AppendTask (chatTask someone)]


//interact :: !d !(l r Bool -> [InteractivePart (!l,!Maybe w)])	!(l r Bool -> InteractiveTerminators a)	!l !(Shared r w) -> Task a

updateSharedInformationA` d (get,putback) stateActions termActions shared 
	= UpdateTask @>> interact
		d
		(\l r=:(ss,os) changed -> 	[ UpdateView (if (l || changed) (FormValue (get r)) (Unchanged (FormValue (get r))),\mbV -> (isJust mbV,fmap (\v -> putback v r) mbV))
						 			: map (\(label,cs) -> Update label (l,Just (ss,cs))) stateActions
						 			])
		(fromPredActions (\valid r changed -> ((valid || changed) && verifyValue (get r),r)) (\action _ r _ -> (action,r)) termActions)
		True
		shared
*/