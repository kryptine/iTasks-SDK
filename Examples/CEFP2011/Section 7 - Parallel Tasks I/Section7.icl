implementation module Section7

// Examples showing the usage of frequently used iTask combinators

import iTasks, Text, StdMisc
from Section3 import show
from Section6 import selectUser, selectUsers

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows7 world

flows7 :: [Workflow]
flows7 
	=   [ workflow "CEFP/Section 7 - Parallel Tasks I/1. Questionnaire"		"Question N users"					(show questions)
	    , workflow "CEFP/Section 7 - Parallel Tasks I/2. Number guessing"	"First person to guess wins"		guess
	    , workflow "CEFP/Section 7 - Parallel Tasks I/3. Naive Chat"		"Naive chat with many users"		naive_chat
		, workflow "CEFP/Section 7 - Parallel Tasks I/4. Monitored Chat" 	"Monitored chat with many users"	monitor_chat
		, workflow "CEFP/Section 7 - Parallel Tasks I/5. Shared Chat"	 	"Shared chat with two users"		shared_chat
		, workflow "CEFP/Section 7 - Parallel Tasks I/6. Multibind Chat" 	"Multibind chat with many users"	multibind_chat
		]
		
// --- some handy functions

normalTask :: !User -> ManagerProperties
normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

const2 :: a b !c -> c
const2 _ _ x = x

noResult _ _ = Void

// A simple application of parallel: all tasks run to completion (generalized variant of exercise 18)
questions :: Task [(User,String)]
questions
	=                  updateInformation "Pose a question" [] "...?"
	  >>= \question -> selectUsers
	  >>= \users    -> parallel "parallel" [] (\_ s -> s) 
	  						[  ShowAs (DetachedTask (normalTask u)) 
	  						          (answer u question) 
	  						\\ u <- users
	  						]
where
	answer u question shared info
		=           updateInformation question [] "...!"
		  >>= \a -> update (\answers -> [(u,a):answers]) shared

// A simple application of parallel: first task to complete terminates parallel (generalized variant of exercise 19)
guess :: Task String
guess
	= return "variant of exercise 19 to do"

// N users chatting with each other
:: ChatState :== [String]

addLine :: User String ChatState -> ChatState
addLine me line s = s ++ [me +++> ": " +++ line]

naive_chat :: Task ChatState
naive_chat
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = join "," (map toString [me : others])
							in  parallel "Naive chat" initChatState (\_ chat -> chat)
								   [  ShowAs (DetachedTask (normalTask who)) (chat names who)
								   \\ who <- [me : others]
								   ]
where
	chat :: String User (Shared ChatState) info -> Task ChatState
	chat names me chatState info
		= forever (              get chatState
		      >>= \xs         -> updateInformation headerEditor [] (Display xs, Note "")
		      >>= \(_,Note a) -> update (addLine me a) chatState
		  )
	where
		headerEditor	= "Chat with " +++ names

	initChatState :: ChatState
	initChatState = []


//	N users chatting with each other, now with monitor task
monitor_chat :: Task ChatState
monitor_chat
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = join "," (map toString [me : others])
							in  parallel "Monitored chat" initChatState (\_ chat -> chat)
								   [  ShowAs (DetachedTask (normalTask who)) (chat names who)
								   \\ who <- [me : others]
								   ]
where
	chat :: String User (Shared ChatState) info -> Task ChatState
	chat names me chatState info
		= (showSharedInformation headerMonitor [] chatState Void) ||- (forever enterLine)
	where
		headerEditor	= "Chat with "       +++ names
		headerMonitor	= "Conversation of " +++ names
		enterLine		=                  enterInformation headerEditor []
						  >>= \(Note a) -> update (addLine me a) chatState

	initChatState :: ChatState
	initChatState = []

:: ChatState2 = { typing	:: [Bool]   	// is chatter i busy with typing
			    , history	:: [String]		// chats so far ....
			    }
derive class iTask ChatState2

initChatState2 n = { typing = repeatn n False, history = []}

setTyping n "" state 	= {state & typing  = updateAt n False state.typing}
setTyping n _ state 	= {state & typing  = updateAt n True  state.typing}

addHistory u lines state = {state & history = state.history ++ [u +++> ": " +++ l \\ l <- lines]}

shared_chat :: Task ChatState2
shared_chat
    =   					get currentUser
    	>>= \me ->			selectUser
		>>= \you ->			parallel "2 Chatters" (initChatState2 2) (\_ s -> s)
								[ShowAs (DetachedTask (normalTask me) ) (chatEditor (me,0) (you,1))
								,ShowAs (DetachedTask (normalTask you)) (chatEditor (you,1) (me,0))
								]
where
	chatEditor :: (User,Int) (User,Int) (Shared ChatState2) (ParallelInfo ChatState2) -> Task Void
	chatEditor (me,mine) (you,yours) cs os
		= 					updateSharedInformation ("Chat with " <+++ you) [chatView,entryView] cs (Note "")
			>?*				[(ActionQuit, Always (return Void))]
	where
		chatView	= ShowView (GetShared toView)
		where
			toView state
				= (you +++> if (state.typing!!yours) " is typing..." " is waiting...", state.history)
		
		entryView	= UpdateView (GetLocal id, Putback fromView)
		where
			fromView (Note response) _ state 
				= case split "\n" response of
					[line]		= (Nothing, Just (setTyping mine response state))
					lines
						# newlines	= init lines
						# newentry	= last lines
						= (Just (Note newentry), Just (setTyping mine newentry (addHistory me newlines state)))
				
// N users chatting with each other

multibind_chat :: Task ChatState
multibind_chat 
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = join "," (map toString [me : others])
							in  parallel "Multibind chat" initChatState (const2 Void)
								   [  ShowAs (DetachedTask (normalTask who)) (chat names who)
								   \\ who <- [me : others]
								   ]
where
	chat :: String User (Shared ChatState) (ParallelInfo ChatState) -> Task Void
	chat names me chatState os
		= (showSharedInformation headerMonitor [] chatState Void) ||- enterLine
	where
		headerEditor	= "Chat with "       +++ names
		headerMonitor	= "Conversation of " +++ names
		enterLine		=     	enterInformation headerEditor []
							>?* [ (ActionQuit, Always (return True))
								, (ActionOk,   IfValid (\(Note a) ->	 update (addLine me a) chatState
																	 >>| return False))
								]
						  	>>= \stop -> if stop (set os [StopParallel] >>| return Void) enterLine

	initChatState :: ChatState
	initChatState = []
