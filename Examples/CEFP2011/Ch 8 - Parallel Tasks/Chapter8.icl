implementation module Chapter8

// Examples showing the usage of frequently used iTask combinators

import iTasks, StdMisc
from Chapter7 import selectUser

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows8 world

flows8 :: [Workflow]
flows8 =  [w0, w1, w2]

w0 = workflow "CEFP/Chap 7/0. Simple Chat"	"Chat with one iTask user, no menus"			chat0
w1 = workflow "CEFP/Chap 7/1. Chat" 		"Chat with one iTask user" 						chat1
w2 = workflow "CEFP/Chap 7/2. Shared Chat" 	"Chat with one iTask user, updating views" 		chat2

// --- some handy functions

normalTask :: !User -> ManagerProperties
normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

const2 :: a b !c -> c
const2 _ _ x = x

noResult _ _ = Void


// 2 users chatting with each other
chat0
    =               		get currentUser
    	>>= \me ->			selectUser
		>>= \you ->			parallel "2 Chatters" [(me,"enter chat"),(you,"enter chat")] (\_ chat -> chat)
								[DetachedTask (normalTask me)  noMenu (chaT me you)
								,DetachedTask (normalTask you) noMenu (chaT you me)
								]
where
	chaT :: User User (SymmetricShared [(User,String)]) parinfo -> Task Void
	chaT me you state parinfo
		=               get state
		  >>= \c     -> updateInformation ("Chat with " <+++ you) (Display c,"")
		  >>= \(_,l) -> update (\s -> s ++ [(me,l)]) state
		  >>| chaT me you state parinfo

// 2 users chatting with each other

:: ChatState :== [String]

initChatState :: ChatState
initChatState = []

chat1 
    =               		get currentUser
    	>>= \me ->			selectUser
		>>= \you ->			parallel "2 Chatters" initChatState (const2 Void)
								[DetachedTask (normalTask me)  noMenu (chatEditor me you)
								,DetachedTask (normalTask you) noMenu (chatEditor you me)
								]
where
	chatEditor :: User User (SymmetricShared ChatState) (ParallelInfo ChatState) -> Task Void
	chatEditor me you chatState os
		= 				(	monitor ("Chat list view") id (const False) False chatState
						  	||-
							(	enterInformationA ("Chat with " <+++ you) 
								>?* [ (ActionQuit, Always (return True))
									, (ActionOk,   IfValid (\(Note a) ->		update (\s -> s ++ [me +++> ": " +++ a]) chatState
																			>>|	return False))
									]
							)
						 )
			>>= \stop -> if stop (set os [StopParallel] >>| return Void) (chatEditor me you chatState os) 

// example, chat using modification of shared state, works funny with current implementation....

chat2 
    =   					get currentUser
    	>>= \me ->			selectUser
		>>= \you ->			parallel "2 Chatters" initChatState (const2 Void)
								[DetachedTask (normalTask me)  noMenu (chatEditor me you)
								,DetachedTask (normalTask you) noMenu (chatEditor you me)
								]
where
	chatEditor me you chatState os
		= 	updateSharedInformationA ("Chat with " <+++ you) (view me) chatState actions 

	view user 
		=	( \list -> (Display list,Note "")
			, \(_,Note response) list -> list ++ [user +++> ": " +++> response]
			)

	actions _ = [(ActionQuit, Just  Void)]
