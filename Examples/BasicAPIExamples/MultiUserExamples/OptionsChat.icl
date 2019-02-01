implementation module BasicAPIExamples.MultiUserExamples.OptionsChat

import iTasks

import iTasks.UI.Definition

import iTasks.Extensions.Admin.UserAdmin
import iTasks.Extensions.DateTime
import iTasks.Extensions.Document

wf :: String -> Workflow
wf a = workflow a "Chat with options" genChat

main :: Task ()
main = multiUserExample @! ()

multiUserExample
	=				allTasks (map (createUser o mkUserAccount) logins)
	>>|				viewInformation "Login under one of the following names (password = login name)" []
						(foldl (+++) "" (map (\n -> n +++ ", ") logins))
					-||-
					viewInformation "and then Select \"new\" to create a new Task..." [] ""
	>>|				installWorkflows [wf "Chat with options"]
	>>|				loginAndManageWork "Chat_4_2 Example"
where
	mkUserAccount name
		= {UserAccount| credentials = {Credentials| username = Username name, password = Password name}, title = Nothing, roles = ["manager"] }


// -------------------------------------------------------------------------

// List of users administrated:

logins = ["bob","alice","carol","dave"]

// ---------------------

:: ChatOptions
		= Text			String
		| DocWithText	(Document, String)
		| Chats		    [ChatMsg ChatOptions]

:: ChatMsg a
		= 	{ time 		:: Time
			, user 		:: String
			, message 	:: a
			}

derive class iTask ChatOptions, ChatMsg


genChat :: Task [ChatMsg ChatOptions]
genChat = createChatSession myChat updateChat

createChatSession :: (Task a) (User a -> Task b) -> Task [b] | iTask a & iTask b
createChatSession enter update
   =           		get currentUser
   >>= \me ->  		enterMultipleChoiceWithShared ("select chatters") [ChooseFromCheckGroup id] users
   >>= \others -> 	withShared [] (startChats enter update [me:others])
where
	startChats :: (Task a) (User a -> Task b) [User] (Shared [b]) -> Task [b] | iTask a & iTask b
	startChats enter update chatters chatStore
		= 	allTasks[(user,foldl (+++) "" (map toString chatters)) @: chatWith user enter update chatStore \\ user <- chatters]
		>>| get chatStore

	chatWith :: User (Task a) (User a -> Task b) (Shared [b]) -> Task () | iTask a & iTask b
	chatWith me enter update chatStore
		=  	viewSharedInformation ("Chat History:") [] chatStore
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


myChat
	=			enterChoice "select message kind" [] ["Text","Doc + Text","NewChat"]
	>>= \sel -> case sel of
				"Text" 			-> oneChat	@ Text o ((+++) "\t")
				"Doc + Text"  	-> oneChat	@ DocWithText
				"NewChat" 		-> genChat	@ Chats
where
	oneChat :: Task a | iTask a
	oneChat = enterInformation "Type in a message: " []

updateChat :: User a -> Task (ChatMsg a) | iTask a
updateChat user chat
	= 		 	  get currentTime
	>>= \time ->  return {time = time, user =  toString user, message = chat}


