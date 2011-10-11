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
	=   [ workflow "CEFP/Section 8 - Parallel Tasks II/1. Chat with several users" 	"Chat with several users" 			chat3
		, workflow "CEFP/Section 8 - Parallel Tasks II/2. Editing a text file" 		"Editing a text file" 				editorApplication
 		, workflow "CEFP/Section 8 - Parallel Tasks II/3. Petition" 				"Invite people to sign a petition" 	myPetition
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
updateText f 			=	update (\s -> {s & mytext = f s.mytext}) 
updateReplace b  		=  	update (\s -> {s & replace = b}) 
updateStat b 			=	update (\s -> {s & statistics = b}) 

noReplace s 	= not s.replace
noStatistics s 	= not s.statistics

voidResult _ _ = Void 

normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

ActionReplace 		:== Action "Replace" // "File/Replace" 
ActionStatistics	:== Action "Statistics" // "File/Statistics" 

editorApplication ::  Task Void
editorApplication 
	=						enterInformation "Give name file to edit..." []
		>>= \fileName ->	readTextFile fileName
		>>= \(_,text) -> 	parallel "Editor" (initEditorState text) voidResult [(BodyTask, editor fileName)]

editor :: String (TaskList EditorState) -> Task ParallelControl
editor fileName ls
	= 			updateSharedInformation (fileName,"Edit " +++ fileName) views myState Void
		>?* 	[ (Action "Save", 		IfValid	save)
		  		, (Action "Quit",		Always 	quit)
		  		, (ActionReplace,	Sometimes (onlyIf (noReplace o fst) replace))
		  		, (ActionStatistics,Sometimes (onlyIf (noStatistics o fst) statistics))
		  		]
where	
	myState = taskListState ls

	views = [UpdateView ( GetShared (\s -> Note s.mytext)
					    , PutbackShared (\(Note text) _ s -> {s & mytext = text}) 
					    )
			] 

	save (val,_)
		=		saveTextFile fileName val.mytext
			>>|	editor fileName ls
	quit
		=		 return Stop

	replace _
		=		updateReplace True myState
			>>| appendTask (DialogTask "Find and Replace", replaceTask {search = "", replaceBy = ""}) ls
			>>| editor fileName ls

	statistics _
		=		updateStat True myState
			>>|	appendTask (DialogTask "Statistics", statisticsTask) ls
			>>| editor fileName ls

replaceTask :: Replace (TaskList EditorState) -> Task ParallelControl
replaceTask replacement ls
	=			updateInformation ("Replace","Define replacement...") [] replacement
		>?*		[(ActionOk,   IfValid replace)
				,(Action "Close", Always close)
				]
where
	myState = taskListState ls

	replace repl
		=		updateText (replaceSubString repl.search repl.replaceBy) myState
			>>|	replaceTask repl ls
	close
		=		updateReplace False myState 
			>>| return Continue


statisticsTask :: (TaskList EditorState) -> Task ParallelControl
statisticsTask ls
	= 			showSharedInformation ("Statistics","Statistics of your document") views myState Void
		>?*		[(Action "Close", Always close)]
where
	myState = taskListState ls

	views = [ShowView (GetShared showStatistics)]

	showStatistics state 
		=	{ lines 	 = length (split "\n" state.mytext)
			, words 	 = length (split " " (replaceSubString "\n" " " state.mytext))
			, characters = textSize state.mytext
			}
	close
		=		updateStat False myState 
			>>| return Continue

// --- file access utility

import StdFile

saveTextFile ::  FileName String -> Task Bool
saveTextFile  fileName text 
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



// invite people to sign a petition

:: Petition	 =	{ titlePetition			:: String
				, deadlineSubmission	:: DateTime
				, description			:: Note 
				}
:: Signer	 =	{ name			:: String
				, profession	:: Maybe String
				, emailAddress	:: String
				, comments		:: Maybe Note
				}

derive class iTask Petition, Signer

myPetition :: Task (Petition,[Signer]) 
myPetition	=  					 enterInformation "Describe the petition" []
				>>= \petition -> campaign petition petition.titlePetition petition.deadlineSubmission

campaign :: petition String DateTime -> Task (petition,[signed]) | iTask petition & iTask signed 
campaign petition title deadline 
	= 						enterSharedMultipleChoice "Invite people to sign" [] users
		>>= \signers ->	parallel ("Sign Petition: " +++ title) [] (\_ signed -> (petition,signed)) 
							[(HiddenTask, waitForDeadline deadline)
							:[(DetachedTask (normalTask signer), sign petition) \\  signer <- signers]
							]
		>>=					showInformation "The petition is signed by:" []
where
	waitForDeadline dateTime list
		=					waitForDateTime dateTime
			>>|				return Stop


sign :: petition (TaskList [signed]) -> Task ParallelControl | iTask petition & iTask signed 
sign petition list
	=					enterInformation ("Please sign the following petition:")  [About petition] 
		>?* 			[(Action "Cancel",	Always 	(return Continue))
	  					,(Action "Sign",	IfValid signAndAskFriends)
	  					]
where
	signAndAskFriends signed
		=			update (\list -> [signed:list]) (taskListState list)
			>>|		showInformation "Thanks for signing !" [] Void
			>>|		enterSharedMultipleChoice "Invite other people too" [] users
			>>= 	askSigners

	askSigners [] 	  = return Continue
	askSigners [c:cs] =  appendTask (DetachedTask (normalTask c), sign petition) list 
						 >>| askSigners cs

