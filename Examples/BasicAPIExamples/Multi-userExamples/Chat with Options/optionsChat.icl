module optionsChat

import iTasks

import iTasks.UI.Definition

import iTasks.Extensions.Admin.UserAdmin
import iTasks.Extensions.DateTime
import iTasks.Extensions.Document


adminTask   :== "Admin/"

Start :: *World -> *World
Start world 
	= startEngine multiUserExample world

multiUserExample
	=				set (map mkUserAccount logins) userAccounts
	>>|				viewInformation "Login under one of the following names (password = login name)" [] 
						(foldl (+++) "" (map (\n -> n +++ ", ") logins)) 
					-||-
					viewInformation "and then Select \"new\" to create a new Task..." [] ""	
	>>|				loginAndManageWorkList "Chat_4_2 Example" [workflow "chat with options" "chat with options" genChat] 
where
	mkUserAccount name  
		= { credentials = { username = Username name, password = Password name}, title = Nothing, roles = ["manager"] }


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


