implementation module Chapter8

// Examples showing the usage of frequently used iTask combinators

import iTasks, Text, StdMisc
from Chapter7 import selectUser, selectUsers

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows8 world

flows8 :: [Workflow]
flows8 =  [w1, w2, w3, w4]

w1 = workflow "CEFP/Chap 8/1. Naive Chat"		"Naive chat with many users"		naive_chat
w2 = workflow "CEFP/Chap 8/2. Monitored Chat" 	"Monitored chat with many users"	monitor_chat
w3 = workflow "CEFP/Chap 8/3. Shared Chat"	 	"Shared chat with many users"		shared_chat
w4 = workflow "CEFP/Chap 8/4. Multibind Chat" 	"Multibind chat with many users"	multibind_chat

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

initChatState :: ChatState
initChatState = []

naive_chat :: Task ChatState
naive_chat
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = join "," (map toString [me : others])
							in  parallel "Naive chat" initChatState (\_ chat -> chat)
								   [  DetachedTask (normalTask who) noMenu (chat names who)
								   \\ who <- [me : others]
								   ]
where
	chat :: String User (SymmetricShared ChatState) info -> Task ChatState
	chat names me chatState info
		= forever (              get chatState
		      >>= \xs         -> updateInformation headerEditor (Display xs,Note "")
		      >>= \(_,Note a) -> update (addLine me a) chatState
		  )
	where
		headerEditor	= "Chat with " +++ names


//	N users chatting with each other, now with monitor task
monitor_chat :: Task ChatState
monitor_chat
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = join "," (map toString [me : others])
							in  parallel "Monitored chat" initChatState (\_ chat -> chat)
								   [  DetachedTask (normalTask who) noMenu (chat names who)
								   \\ who <- [me : others]
								   ]
where
	chat :: String User (SymmetricShared ChatState) info -> Task ChatState
	chat names me chatState info
		= (monitor headerMonitor id (const False) False chatState) ||- (forever enterLine)
	where
		headerEditor	= "Chat with "       +++ names
		headerMonitor	= "Conversation of " +++ names
		enterLine		=                  enterInformation headerEditor
						  >>= \(Note a) -> update (addLine me a) chatState

// example, chat using modification of shared state, works funny with current implementation....
shared_chat :: Task ChatState
shared_chat
    =   					get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = join "," (map toString [me : others]) 
							in  parallel "Shared chat" initChatState (const2 Void)
								   [  DetachedTask (normalTask who) noMenu (chat names who)
								   \\ who <- [me : others]
								   ]
where
	chat names me chatState os
		= 	updateSharedInformationA headerEditor (view me) chatState actions
	where
		headerEditor	= "Chat with " +++ names
		view me 		=	( \xs -> (Display xs,Note "")
							, \(_,Note a) -> addLine me a
							)
		actions _		= [(ActionQuit,Just Void)]

// N users chatting with each other
multibind_chat :: Task ChatState
multibind_chat 
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = join "," (map toString [me : others])
							in  parallel "Multibind chat" initChatState (const2 Void)
								   [  DetachedTask (normalTask who) noMenu (chat names who)
								   \\ who <- [me : others]
								   ]
where
	chat :: String User (SymmetricShared ChatState) (ParallelInfo ChatState) -> Task Void
	chat names me chatState os
		= (monitor headerMonitor id (const False) False chatState) ||- enterLine
	where
		headerEditor	= "Chat with "       +++ names
		headerMonitor	= "Conversation of " +++ names
		enterLine		=     enterInformationA headerEditor 
								>?* [ (ActionQuit, Always (return True))
									, (ActionOk,   IfValid (\(Note a) ->	 update (addLine me a) chatState
																		 >>| return False))
									]
						  >>= \stop -> if stop (set os [StopParallel] >>| return Void) enterLine
