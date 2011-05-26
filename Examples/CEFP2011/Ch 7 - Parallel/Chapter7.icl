implementation module Chapter7

// Examples showing the usage of frequently used iTask combinators

import iTasks, StdMisc

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine 	[ 
							] world

flows7 :: [Workflow]
flows7 =  [w1, w2]

w1 = workflow "CEFP/Chap 7/1. Chat" 		"Chat with one iTask user" 						chat1
w2 = workflow "CEFP/Chap 7/2. Shared Chat" 	"Chat with one iTask user, updating views" 		chat2

// --- some handy functions

normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

mapMaybe ::  (a -> b) (Maybe a) -> Maybe b
mapMaybe f (Just a)  = Just (f a)
mapMaybe _ Nothing  = Nothing

noResult _ _ = Void


selectUser :: Task User
selectUser
		= 					get users
			>>=				enterChoice "Select a user:"


// 2 users chatting with each other

:: ChatState :== [String]

initChatState :: ChatState
initChatState = []

chat1 
    =               		get currentUser
    	>>= \me ->			selectUser
		>>= \you ->			parallel "2 Chatters" initChatState noResult
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
		>>= \you ->			parallel "2 Chatters" initChatState noResult
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
	
	
	