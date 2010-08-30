implementation module Chat

import StdList, StdOrdList, StdTuple, StdMisc
import iTasks
import Text
import CommonDomain

derive class iTask	Chat, ChatMessage, ChatView, ChatMessageView
derive gMerge		Chat, ChatMessage, ChatView, ChatMessageView
derive bimap		(,), Maybe

chatExample :: [Workflow]
chatExample
=	[ workflow	 "Examples/Applications/Chat" chat ]

//Shared State	
:: Chat =
	{ initUser	:: User
	, users		:: [User]
	, messages	:: [ChatMessage]
	}
	
:: ChatMessage =
	{ who		:: User
	, when		:: DateTime
	, message	:: Note
	, replies	:: [ChatMessage]
	}

//Transformed View
:: ChatView = 
	{ users		:: Display [User]
	, messages	:: Display [ChatMessageView]
	}
	
:: ChatMessageView = 
	{ info		:: String
	, message 	:: VisualizationHint Note
	, replies	:: [ChatMessageView]
	, addReply	:: Editable FormButton
	}
	
ActionAddUser :== ActionLabel "Add User" 

chat
	=				getCurrentUser
	>>= \me ->		selectFriends
	>>= \friends -> createChatBox me
	>>= \chatbox ->	allTasks ([spawnProcess True True (f @>> Subject "Chat Request" @>> (initiateChat chatbox f [me:friends])) \\ f <- friends]
							++ [spawnProcess True True (me @>> Subject "Chat Request" @>> (initSession >>| chatSession chatbox (me)))]) 						
where
	
	createChatBox :: User -> (Task (DBId Chat))
	createChatBox me = createDB {Chat | initUser = me, users = [], messages = []}

	selectFriends :: Task [User]
	selectFriends = enterInformation "Select friends" "Whom do you want to chat with?"
	
	initiateChat :: (DBId Chat) User [User] -> Task Void
	initiateChat chatbox friend friends
		=	requestConfirmation "Confirm" ("Do you want to initiate a chat with "+++printFriends+++"?")
		>>= \yes -> if yes
						(initSession >>| chatSession chatbox friend)
						(return Void)
	where
		printFriends = join ", " (map toString friends)

	initSession :: Task Void
	initSession = setMenus
		[ Menu "File" [ MenuItem "New Topic" ActionNew		Nothing
					  , MenuItem "Add Users" ActionAddUser	Nothing
					  , MenuItem "Quit"		 ActionQuit		Nothing
					  ]
		]
	
	chatSession :: (DBId Chat) User -> Task Void
	chatSession chatbox user 
		= 			readDB chatbox
		>>= \chat -> writeDB chatbox {Chat | chat & users = chat.Chat.users++[user]}
		>>|	dynamicGroupAOnly [chatEditor chatbox user <<@ GBAlwaysFixed] (chatActions chatbox user)
	where
		chatActions :: (DBId Chat) User -> [GroupAction GOnlyAction Void Chat]
		chatActions chatbox user = [ GroupAction	ActionNew		(GOExtend [newTopic chatbox user >>| stop]) 	GroupAlways
						 		   , GroupAction 	ActionQuit		GOStop											GroupAlways
						 		   , GroupAction	ActionAddUser	(GOExtend [addUsers chatbox >>| stop])			(SharedPredicate chatbox (\(SharedValue chat) -> chat.Chat.initUser == user)) 
						 		   ]
						 		   	
	chatEditor :: (DBId Chat) User -> Task Void
	chatEditor chatbox user = getCurrentDateTime >>= \dt -> updateShared "Chat" "You can chat now" [] chatbox [mainEditor user dt] >>| return Void
	
	mainEditor :: User DateTime -> (View Chat)
	mainEditor user dt = editor {editorFrom = editorFrom user, editorTo = editorTo user dt}
	where
		editorFrom :: User Chat -> ChatView
		editorFrom user chat = {ChatView 
							   | users 		= Display chat.Chat.users
							   , messages 	= Display [(convertMessageToView user msg) \\ msg <- chat.Chat.messages]
							   }
		where
			convertMessageToView :: User ChatMessage -> ChatMessageView
			convertMessageToView user msg =
				{ ChatMessageView
				| info		= toString msg.ChatMessage.who+++" said at "+++toString msg.ChatMessage.when
				, message	= if(user == msg.ChatMessage.who) (VHEditable msg.ChatMessage.message) (VHDisplay msg.ChatMessage.message)
				, replies	= [convertMessageToView user reply \\ reply <- msg.ChatMessage.replies]
				, addReply	= Editable {FormButton | label = "Add reply", icon = "", state = NotPressed}
				}
		
		editorTo :: User DateTime ChatView Chat -> Chat
		editorTo user dt view chat = {Chat
								  | chat
								  & messages 	= [convertViewToMessage user dt vmsg omsg \\ vmsg <- (fromDisplay view.ChatView.messages) & omsg <- chat.Chat.messages]
								  }
		where
			convertViewToMessage :: User DateTime ChatMessageView ChatMessage -> ChatMessage
			convertViewToMessage user dt vmsg omsg =
				{ ChatMessage
				| who		= omsg.ChatMessage.who
				, when		= omsg.ChatMessage.when
				, message	= fromVizHint vmsg.ChatMessageView.message
				, replies	= [convertViewToMessage user dt vreply oreply \\ vreply <- vmsg.ChatMessageView.replies & oreply <- omsg.ChatMessage.replies ] ++ addReply (fromEditable vmsg.addReply) user dt
				}
			
			addReply :: FormButton User DateTime -> [ChatMessage]
			addReply button user dt 
				= case button.state of
					Pressed
						= [{ChatMessage | who = user, when = dt, message = Note "", replies = []}]
					NotPressed
						= []
				
			fromVizHint (VHEditable x) 		= x
			fromVizHint (VHDisplay x) 	= x
			fromVizHint (VHHidden x) 		= x
	
	newTopic :: (DBId Chat) User -> Task Void
	newTopic chatbox user 
		= 				readDB  chatbox
		>>= \chat ->	getCurrentDateTime
		>>= \dt	  ->	writeDB chatbox (addNew chat user dt)
		>>|				return  Void
	where
		addNew chat user dt = {Chat | chat & messages = chat.Chat.messages ++ [mkMsg user dt]}			
			
		mkMsg user dt = {ChatMessage 
						| who = user
						, when = dt
		  				, message = (Note "")
		  				, replies = []
		   				}
	
	addUsers :: (DBId Chat) -> Task Void
	addUsers chatbox
		= 			 	enterInformation "Select users" "Select users to add to the chat"	
		>>= \users -> 	readDB chatbox
		>>= \chat ->	allTasks ([spawnProcess True True (u @>> Subject "Chat Request" @>> (initiateChat chatbox u (chat.Chat.users++users))) \\ u <- users])
		>>| 		 	return Void