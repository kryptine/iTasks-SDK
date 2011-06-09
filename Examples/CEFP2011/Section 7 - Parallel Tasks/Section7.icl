implementation module Section7

// Examples showing the usage of frequently used iTask combinators

import iTasks, Text, StdMisc
from Section6 import selectUser, selectUsers

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows7 world

flows7 :: [Workflow]
flows7 =  [w1, w2, w3, w4]

w1 = workflow "CEFP/Sect 7/1. Naive Chat"		"Naive chat with many users"		naive_chat
w2 = workflow "CEFP/Sect 7/2. Monitored Chat" 	"Monitored chat with many users"	monitor_chat
w3 = workflow "CEFP/Sect 7/3. Shared Chat"	 	"Shared chat with many users"		shared_chat
w4 = workflow "CEFP/Sect 7/4. Multibind Chat" 	"Multibind chat with many users"	multibind_chat

// --- some handy functions

normalTask :: !User -> ManagerProperties
normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

const2 :: a b !c -> c
const2 _ _ x = x

noResult _ _ = Void

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
		= (monitor headerMonitor [] chatState) ||- (forever enterLine)
	where
		headerEditor	= "Chat with "       +++ names
		headerMonitor	= "Conversation of " +++ names
		enterLine		=                  enterInformation headerEditor []
						  >>= \(Note a) -> update (addLine me a) chatState

	initChatState :: ChatState
	initChatState = []

:: ChatState2 = { buffer :: [String]
			    , chats  :: [String]
			    }
derive class iTask ChatState2


initChatState2 = { buffer = ["",""], chats = []}


shared_chat :: Task ChatState2
shared_chat
    =   					get currentUser
    	>>= \me ->			selectUser
		>>= \you ->			parallel "2 Chatters" initChatState2 (\_ s -> s)
								[ShowAs (DetachedTask (normalTask me) ) (chatEditor me you 0 1)
								,ShowAs (DetachedTask (normalTask you)) (chatEditor you me 1 0)
								]
where
	chatEditor :: User User Int Int (Shared ChatState2) (ParallelInfo ChatState2) -> Task Void
	chatEditor me you mine yours cs os
//		= 					updateSharedInformation ("Chat with " <+++ you) [] cs  
/*		=					get cs
			>>= \state ->	updateInformation ("Chat with " <+++ you) [] (Display state.chats, Note "")
			>>= \(_,Note response) -> update (\state -> { state &  chats = state.chats ++ [me +++> ": " +++> response]}) cs
			>>|				chatEditor me you mine yours cs os

*/		= 					updateSharedInformation ("Chat with " <+++ you) [View view] cs  
			>?*				[(ActionQuit, Always (return Void))]
	where
		view 
			=	( \state			               -> ( Display (you +++> if (state.buffer!!yours <> "") 
																				" is typing..."  
																				" is waiting...")
													  , Display state.chats
													  , Note (state.buffer!!mine)
													  )
				, \(_,_,Note response) state -> 	handleResponse response state
				)
		where
			handleResponse response state 
			# responseList 		= fromString response
			| not (isMember '\n' responseList)	= {state & buffer = updateAt mine response state.buffer}
			# (before,after) 	= span (\c -> c <> '\n') responseList 
			# beforeS 			= toString before
			# afterS 			= toString (tl after) 
			= { state & buffer	= updateAt mine afterS state.buffer
					  , chats	= state.chats ++ [me +++> ": " +++> beforeS]
			  }

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
		= (monitor headerMonitor [] chatState) ||- enterLine
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
