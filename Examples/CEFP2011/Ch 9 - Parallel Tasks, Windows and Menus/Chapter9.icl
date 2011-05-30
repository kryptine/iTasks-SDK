implementation module Chapter9

// Examples showing the usage of editors with multiple buttons

import iTasks
from Chapter7 import selectUser
from Chapter8 import normalTask, const2

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows9 world

flows9 :: [Workflow]
flows9 =  [w1, w2]

w1 = workflow "CEFP/Chap 9/1. Chat with several users"    	"Chat with several users" chat3
w2 = workflow "CEFP/Chap 9/2. Arrange a meeting date between several users" "Arrange meeting" mkAppointment


// chat with several users

derive class iTask ChatState, Message

:: ChatState	=	{ chatters  :: [User]
				    , chats		:: [Message]
				    }
:: Message	=		{ chatting	:: User
					, message	:: String
					}
emptyChatState = {chatters = [], chats = []}

addMessage user message cs = {cs & chats = cs.chats ++ [{chatting = user, message = message}]}
addUser user			cs = {cs & chatters = [user:cs.chatters]}
removeUser user			cs = {cs & chatters = removeMember user cs.chatters}

chat3
    =               get currentUser
    	>>= \me ->	parallel "Chat application" emptyChatState (const2 Void) [InBodyTask (chatTask me)]

chatTask user cs os
	=			update (addUser user) cs
		>>|		monitor ("Chat list view") id (const False) False cs
				||- 
				chatMore user "" cs os

chatMore user s cs os 
	= 	updateInformationA ("Chat with iTask users") (toView,fromView) s  	
		>?*	[(ActionAdd,  IfValid (\r  ->	  set os [AppendTask newChatter]
										  >>| chatMore user r cs os))
			,(ActionOk,   IfValid (\r  ->	  update (addMessage user r) cs 
										  >>| chatMore user "" cs os))
			,(ActionQuit, Always (			  update (removeUser user o addMessage user "bye") cs 
										  >>| return Void	))
			]
where		
	(toView, fromView) = (\c -> Note c, \(Note c) _ -> c) 

newChatter = WindowTask "Append Chatter" noMenu handleNewChatter

handleNewChatter cs os
	=						selectUser
		>>= \someone ->		set os [AppendTask (newChatTask someone)]
where
	newChatTask someone = DetachedTask (normalTask someone) noMenu (chatTask someone)


ActionAdd :== Action "Add Chatter" "Add Chatter"

// pocket calculator, see Steffens example...


// making an appointment

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
	=	{ date 		:: Display Date
		, time		:: Display Time
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
//		meetingTask :: (SymmetricShared [MeetingProposal]) (ParallelInfo [MeetingProposal]) -> Task [MeetingProposal]
		meetingTask meetingState _
			= updateSharedInformationA "When can we meet ?" (viewForUser user,modelFromView) meetingState noActions 

		managerProperties
			= { worker = user, priority	= NormalPriority, deadline = Nothing, status = Active }	
	
	viewForUser :: User [MeetingProposal] -> [MeetingProposalView]
	viewForUser user props
		= [  {MeetingProposalView | date=toDisplay date, time=toDisplay time, canMeet=map (viewParticipant user) canMeet}
		  \\ {MeetingProposal | date,time,canMeet} <- props
		  ]
	
	modelFromView :: [MeetingProposalView] .a -> [MeetingProposal]
	modelFromView props _
		= [  {MeetingProposal | date=fromDisplay date, time=fromDisplay time, canMeet=map modelParticipant canMeet} 
		  \\ {MeetingProposalView | date,time,canMeet} <- props
		  ]
	
	viewParticipant :: User Participant -> VisualizationHint ParticipantView
	viewParticipant user participant=:{Participant | name, canAttend, comment}
		= if (user == name)	VHEditable VHDisplay view
	where
		view		= {ParticipantView | name      = Display name
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
			=     				updateSharedInformationA "Monitor answers" (viewForManager,\_ ps -> ps)  meetingState 
				>?*	 			[(ActionOk,IfValid return)]
			  	>>= \props -> enterChoice "Choose meeting" props
		where
			viewForManager :: [MeetingProposal] -> [MeetingProposal]
			viewForManager props
				= [ p \\ p=:{MeetingProposal | canMeet=can} <- props | and [canAttend \\ {Participant | canAttend} <- can] ]
