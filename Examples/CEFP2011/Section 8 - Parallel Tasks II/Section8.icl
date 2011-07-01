implementation module Section8

// Examples showing the usage of editors with multiple buttons

import iTasks
from Section4 import onlyIf
from Section6 import selectUser
from Section7 import normalTask, const2

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows8 world

flows8 :: [Workflow]
flows8 
	=   [ workflow "CEFP/Section 8 - Parallel Tasks II/1. Chat with several users"    	"Chat with several users" 	chat3
		, workflow "CEFP/Section 8 - Parallel Tasks II/2. Editing a text file" 			"Editing a text file" 		editorApplication
//		, workflow "CEFP/Section 8 - Parallel Tasks II/3. Arrange a meeting date between several users" "Arrange meeting" mkAppointment
		]

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
    	>>= \me ->	parallel "Chat application" emptyChatState (const2 Void) [(BodyTask, chatTask me)]

chatTask user cs
	=			update (addUser user) (taskListState cs)
		>>|		showSharedInformation ("Chat list view") [] (taskListState cs) Void
				||- 
				chatMore user "" cs

chatMore user s cs 
	= 	updateInformation ("Chat with iTask users") [UpdateView (GetLocal toView,PutbackLocal fromView)] s  	
		>?*	[(ActionAdd,  IfValid (\r  ->	  appendTask newChatter cs
										  >>| chatMore user r cs))
			,(ActionOk,   IfValid (\r  ->	  update (addMessage user r) (taskListState cs) 
										  >>| chatMore user "" cs))
			,(ActionQuit, Always (			  update (removeUser user o addMessage user "bye") (taskListState cs) 
										  >>| return Stop	))
			]
where		
	toView c =  Note c 
	fromView (Note c) _ _ = c

newChatter = (WindowTask "Append Chatter", handleNewChatter)

handleNewChatter cs
	=						selectUser
		>>= \someone ->		appendTask (newChatTask someone) cs
		>>|					return Continue
where
	newChatTask someone = (DetachedTask (normalTask someone), chatTask someone)


ActionAdd :== Action "Add Chatter" 

// pocket calculator, see Steffens example...


import Text

derive class iTask Replace, TextStatistics, EditorState

:: Replace			=	{ search 		:: String
						, replaceBy 	:: String
						}
:: TextStatistics 	=	{ lines			:: Int
						, words			:: Int
						, characters	:: Int
						}
:: EditorState		=	{ mytext		:: String
						, replace		:: Bool
						, statistics	:: Bool
						}

:: FileName		:== String

initEditorState text 	= 	{mytext = text, replace = False, statistics = False}
updateReplace b  		=  	update (\s ->{s & replace = b}) 
updateStat b 			=	update (\s -> {s & statistics = b}) 
updateText f 			=	update (\s -> {s & mytext = f s.mytext}) 

voidResult _ _ = Void 

normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

ActionReplace 		:== Action "File/Replace" 
ActionStatistics	:== Action "File/Statistics" 

editorApplication ::  Task Void
editorApplication 
	=						enterInformation "Give name of text file you want to edit..." []
		>>= \fileName ->	readTextFile fileName
		>>= \(_,text) -> 	parallel "Editor" (initEditorState text) voidResult [(BodyTask, editor fileName)]

editor :: String (TaskList EditorState) -> Task ParallelControl
editor fileName ls
	= 			updateSharedInformation (fileName,"Edit text file \"" +++ fileName +++ "\"") views (taskListState ls) Void
		>?* 	[ (ActionSave, 		IfValid	save)
		  		, (ActionQuit,		Always 	quit)
		  		, (ActionReplace,	Sometimes (onlyIf (\(s,_) -> not s.replace)    replace))
		  		, (ActionStatistics,Sometimes (onlyIf (\(s,_) -> not s.statistics) statistics))
		  		]
where	
	views = [UpdateView ( GetShared (\s -> Note s.mytext)
					    , PutbackShared (\(Note text) _ s -> {s & mytext = text}) 
					    )
			]

	save (val,_)
		=		safeTextFile fileName val.mytext
			>>|	editor fileName ls
	quit
		=		 return Stop

	replace _
		=		updateReplace True (taskListState ls)
			>>| appendTask (DialogTask "Find and Replace", replaceTask {search = "", replaceBy = ""}) ls
			>>| editor fileName ls

	statistics _
		=		updateStat True (taskListState ls)
			>>|	appendTask (DialogTask "Statistics", statisticsTask) ls
			>>| editor fileName ls

replaceTask :: Replace (TaskList EditorState) -> Task ParallelControl
replaceTask replacement ls
	=			updateInformation ("Replace","Define replacement...") [] replacement
		>?*		[(ActionOk,   IfValid replace)
				,(Action "Close", Always close)
				]
where
	replace repl
		=		updateText (replaceSubString repl.search repl.replaceBy) (taskListState ls)
			>>|	replaceTask repl ls
	close
		=		updateReplace False (taskListState ls) 
			>>| return Continue


statisticsTask :: (TaskList EditorState) -> Task ParallelControl
statisticsTask ls
	= 			showSharedInformation ("Statistics","Statistics of your document") views (taskListState ls) Void
		>?*		[(Action "Close", Always close)]
where
	views = [ShowView (GetShared showStatistics)]

	showStatistics state 
		=	{ lines 	 = length (split "\n" state.mytext)
			, words 	 = length (split " " (replaceSubString "\n" " " state.mytext))
			, characters = textSize state.mytext
			}
	close
		=		updateStat False (taskListState ls) 
			>>| return Continue

// --- file access utility

import StdFile

safeTextFile ::  FileName String -> Task Bool
safeTextFile  fileName text 
	= 						accWorld (safeFileMonad  fileName text)
where
	safeFileMonad ::  String String *World -> (Bool,*World)
	safeFileMonad  fileName text world 
	# (ok,file,world)  	= fopen fileName FWriteText world
	| not ok			= (False,world)
	# file				= fwrites text file
	= fclose file world

readTextFile ::  FileName  -> Task (Bool,String)
readTextFile  fileName  
	= 						accWorld (readFileMonad fileName)
where
	readFileMonad :: String  *World -> ((Bool,String),*World)
	readFileMonad fileName world 
	# (ok,file,world)  	= fopen fileName FReadText world
	| not ok			= ((False,""),world)
	# (text,file)		= freads file 1000000
	| text == ""		= ((False,""),world)
	# (ok,world)		= fclose file world
	| not ok			= ((False,""),world)
	= ((True,text),world)

/*

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
		>>= \all ->		enterMultipleChoice "Who should attend the meeting ?" [] all
		>>= \users ->	enterInformation "Propose meeting dates" []
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
		= ShowAs (DetachedTask managerProperties) meetingTask
	where
//		meetingTask :: (SymmetricShared [MeetingProposal]) (ParallelInfo [MeetingProposal]) -> Task [MeetingProposal]
		meetingTask meetingState _
			= updateSharedInformation "When can we meet ?" [View (viewForUser user,modelFromView)] meetingState  

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
		= ShowAs BodyTask check
	where
		check meetingState controlState
			=     				updateSharedInformation "Monitor answers" [View (viewForManager,\_ ps -> ps)]  meetingState 
				>?*	 			[(ActionOk,IfValid return)]
			  	>>= \props -> 	enterChoice "Choose meeting" [] props
		where
			viewForManager :: [MeetingProposal] -> [MeetingProposal]
			viewForManager props
				= [ p \\ p=:{MeetingProposal | canMeet=can} <- props | and [canAttend \\ {Participant | canAttend} <- can] ]
*/