implementation module BasicAPIExamples.MultiUserExamples.Chat

import iTasks

import iTasks.Extensions.Admin.UserAdmin
import iTasks.Extensions.Admin.WorkflowAdmin

wf :: String -> Workflow
wf a = workflow a "Chat with other users" myExample

main :: Task ()
main = myExample @! ()

multiUserExample
	=				allTasks (map (createUser o mkUserAccount) players)
	>>|				viewInformation [ViewWithHint "Login under one of the following names (password = login name)"]
						(foldl (+++) "" (map (\n -> n +++ ", ") players))
					-||-
					viewInformation [ViewWithHint "and then Select \"new\" to create a new Task..."] ""
	>>|				installWorkflows [wf "chat"]	
	>>|				loginAndManageWork "Chat_4_2 Example" Nothing Nothing False
where
	mkUserAccount name
		= {UserAccount| credentials = {Credentials| username = Username name, password = Password name}, title = Nothing, roles = ["manager"] }

// -------------------------------------------------------------------------
// Simple MultiUser Chat Application for 2 users

players = ["bob","alice","carol","dave"]

myExample
	= createChatSession enter update
where
	enter :: Task String
	enter = enterInformation [EnterWithHint "Type in a message"]

	update :: User String -> Task String
	update user chat = return (toString user +++ " says : " +++ chat)

createChatSession :: (Task a) (User a -> Task b) -> Task [b] | iTask a & iTask b
createChatSession enter update
   =           		get currentUser
   >>= \me ->  		enterMultipleChoiceWithShared ("select chatters") [ChooseFromCheckGroup id] users
   >>= \others -> 	withShared [] (startChats enter update [me:others])

startChats :: (Task a) (User a -> Task b) [User] (Shared sds [b]) -> Task [b] | iTask a & iTask b & RWShared sds
startChats enter update chatters chatStore
	= 	allTasks[(user, "chat") @: chatWith user enter update chatStore \\ user <- chatters]
	>>| get chatStore

chatWith :: User (Task a) (User a -> Task b) (Shared sds [b]) -> Task () | iTask a & iTask b & RWShared sds
chatWith me enter update chatStore
	=  	viewSharedInformation [ViewWithHint "Chat History:"] chatStore
	   	||-
		oneChat
where
	oneChat
		=	enter
	    >>* [ OnAction (Action "Send") (hasValue send)
	    	, OnAction (Action "Quit") (always (return ()))
	    	]
	send nchat
		=			 update me nchat
		>>= \new ->  upd (\chats -> chats ++ [new]) chatStore
		>>| 		 oneChat
