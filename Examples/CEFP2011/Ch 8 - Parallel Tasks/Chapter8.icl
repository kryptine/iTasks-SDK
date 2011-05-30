implementation module Chapter8

// Examples showing the usage of frequently used iTask combinators

import iTasks, StdMisc
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

concatWith :: !String ![String] -> String
concatWith sep []	= ""
concatWith sep [x]	= x
concatWith sep xs	= foldr (\a as -> a +++ sep +++ as) (last xs) (init xs)

// N users chatting with each other
:: ChatState :== [String]

addLine :: User String ChatState -> ChatState
addLine me line s = s ++ [me +++> ": " +++ line]

initChatState :: ChatState
initChatState = []

naive_chat
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = concatWith "," (map userName [me : others])
							in  parallel "Naive chat" [""] (\_ chat -> chat)
								   [  DetachedTask (normalTask person) noMenu (chatEditor names person)
								   \\ person <- [me : others]
								   ]
where
	chatEditor :: String User (SymmetricShared ChatState) parinfo -> Task ChatState
	chatEditor names me chatState parinfo
		= forever (              get chatState
		      >>= \list       -> updateInformation headerEditor (Display list,Note "")
		      >>= \(_,Note a) -> update (addLine me a) chatState
		  )
	where
		headerEditor	= "Chat with " +++ names


//	N users chatting with each other, now with monitor task
monitor_chat
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = concatWith "," (map userName [me : others])
							in  parallel "Monitored chat" [] (\_ chat -> chat)
								   [  DetachedTask (normalTask person) noMenu (chatEditor names person)
								   \\ person <- [me : others]
								   ]
where
	chatEditor :: String User (SymmetricShared ChatState) parinfo -> Task ChatState
	chatEditor names me chatState parinfo
		= (monitor headerMonitor id (const False) False chatState) ||- (forever enterLine)
	where
		headerEditor	= "Chat with "       +++ names
		headerMonitor	= "Conversation of " +++ names
		enterLine		=                  enterInformation headerEditor
						  >>= \(Note a) -> update (addLine me a) chatState

// example, chat using modification of shared state, works funny with current implementation....
shared_chat
    =   					get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = concatWith "," (map userName [me : others]) 
							in  parallel "Shared chat" initChatState (const2 Void)
								   [  DetachedTask (normalTask person) noMenu (chatEditor names person)
								   \\ person <- [me : others]
								   ]
where
	chatEditor names me chatState os
		= 	updateSharedInformationA headerEditor (view me) chatState actions
	where
		headerEditor	= "Chat with " +++ names
		view me 		=	( \list -> (Display list,Note "")
							, \(_,Note a) -> addLine me a
							)
		actions _		= [(ActionQuit,Just Void)]

// N users chatting with each other
multibind_chat 
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = concatWith "," (map userName [me : others])
							in  parallel "Multibind chat" initChatState (const2 Void)
								   [  DetachedTask (normalTask person) noMenu (chatEditor names person)
								   \\ person <- [me : others]
								   ]
where
	chatEditor :: String User (SymmetricShared ChatState) (ParallelInfo ChatState) -> Task Void
	chatEditor names me chatState os
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
