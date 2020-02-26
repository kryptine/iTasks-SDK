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
	>-|				(Hint "Login under one of the following names (password = login name)" @>> viewInformation []
						(foldl (+++) "" (map (\n -> n +++ ", ") logins)))
					-||-
					(Hint "and then Select \"new\" to create a new Task..." @>> viewInformation [] "")
	>!|				installWorkflows [wf "Chat with options"]
	>-|				loginAndManageWork "Chat_4_2 Example" Nothing Nothing False
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
   >>- \me ->  		Hint "select chatters" @>> enterMultipleChoiceWithShared [ChooseFromCheckGroup id] users
   >>! \others -> 	withShared [] (startChats enter update [me:others])
where
	startChats :: (Task a) (User a -> Task b) [User] (Shared sds [b]) -> Task [b] | iTask a & iTask b & RWShared sds
	startChats enter update chatters chatStore
		= 	allTasks[(user,foldl (+++) "" (map toString chatters)) @: chatWith user enter update chatStore \\ user <- chatters]
		>-| get chatStore

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


myChat
	=			Hint "select message kind" @>> enterChoice [] ["Text","Doc + Text","NewChat"]
	>>! \sel -> case sel of
				"Text" 			-> oneChat	@ Text o ((+++) "\t")
				"Doc + Text"  	-> oneChat	@ DocWithText
				"NewChat" 		-> genChat	@ Chats
where
	oneChat :: Task a | iTask a
	oneChat = Hint "Type in a message: " @>> enterInformation []

updateChat :: User a -> Task (ChatMsg a) | iTask a
updateChat user chat
	= 		 	  get currentTime
	>>- \time ->  return {time = time, user =  toString user, message = chat}


