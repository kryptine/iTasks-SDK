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
	>-|				(Hint "Login under one of the following names (password = login name)" @>> viewInformation []
						(foldl (+++) "" (map (\n -> n +++ ", ") players)))
					-||-
					(Hint "and then Select \"new\" to create a new Task..." @>> viewInformation [] "")
	>!|				installWorkflows [wf "chat"]
	>-|				loginAndManageWork "Chat_4_2 Example" Nothing Nothing False
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
	enter = Hint "Type in a message" @>> enterInformation []

	update :: User String -> Task String
	update user chat = return (toString user +++ " says : " +++ chat)

createChatSession :: (Task a) (User a -> Task b) -> Task [b] | iTask a & iTask b
createChatSession enter update
	=              get currentUser
	>>- \me ->     Hint "select chatters" @>> enterMultipleChoiceWithShared [ChooseFromCheckGroup id] users
	>>! \others -> withShared [] (startChats enter update [me:others])

startChats :: (Task a) (User a -> Task b) [User] (Shared sds [b]) -> Task [b] | iTask a & iTask b & RWShared sds
startChats enter update chatters chatStore
	= 	allTasks[(user, "chat") @: chatWith user enter update chatStore \\ user <- chatters]
	>?| get chatStore

chatWith :: User (Task a) (User a -> Task b) (Shared sds [b]) -> Task () | iTask a & iTask b & RWShared sds
chatWith me enter update chatStore
	=  	Hint "Chat History:" @>> viewSharedInformation [] chatStore
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
		>>- \new ->  upd (\chats -> chats ++ [new]) chatStore
		>-| 		 oneChat
