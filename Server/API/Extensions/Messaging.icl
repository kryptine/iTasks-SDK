implementation module Messaging

import iTasks
import CommonDomain
import Groups

derive class iTask Message
derive bimap Maybe, (,)

:: Message =
	{ messageId			:: Hidden Int
	, sender			:: HtmlDisplay User
	, recipients 		:: [User]
	, cc 				:: Maybe [User]
	, priority 			:: TaskPriority
	, subject			:: String
	, message			:: Note
	, attachments		:: Maybe [Document]
	, previousMessages 	:: HtmlDisplay [Message]
	}

instance DB Message
where
	databaseId				= mkDBId "Messages"
	getItemId m				= DBRef (fromHidden m.Message.messageId)
	setItemId (DBRef i) m 	= {Message|m & messageId = toHidden i}

mkMsg :: User -> Message
mkMsg me = { Message
			| messageId		= toHidden 0
			, sender 		= toHtmlDisplay me
			, recipients 	= []
			, cc 			= Nothing
			, priority 		= NormalPriority
			, subject		= ""
			, message		= Note ""
		  	, attachments	= Nothing
		   	, previousMessages = HtmlDisplay []
		   	}

manageMessages :: Task Void
manageMessages =
	(	getMyMessages
	>>= overview
	>>= \(action,messages) -> case action of
		ActionOpen						= openMessages messages	>>|	return False
		ActionLabel "New message"		= newMessage			>>|	return False
		ActionLabel "New group message"	= newGroupMessage		>>| return False
		ActionQuit						= 							return True
	) <! id >>| stop
where
	overview :: [Message] -> Task (Action,[Message])
	overview []		= showMessageA "My messages" "You have no messages" [aNew,aNewGroup,aQuit] []
	overview msgs	= enterMultipleChoiceA "My messages" "Your messages:" [aOpen,aNew,aNewGroup,aQuit] msgs
	
	aOpen		= ButtonAction (ActionOpen,IfValid)
	aNew		= ButtonAction (ActionLabel "New message", Always)
	aNewGroup	= ButtonAction (ActionLabel "New group message", Always)
	aQuit		= ButtonAction (ActionQuit,Always)
	
	openMessages :: [Message] -> Task Void
	openMessages messages
		= 	getContextWorker
		>>= \me ->
			allTasks
				[spawnProcess me True True
				 ((Subject ("Message from "+++toString (fromHtmlDisplay msg.Message.sender)+++": "+++msg.Message.subject)) @>>
				  msg.Message.priority @>>
				   readMessage msg)
				 \\ msg <- messages]
		>>| stop
	
newMessage :: Task Void
newMessage = getCurrentUser
	>>= \me -> 		writeMessage me "" [] [] []
	>>= \msg -> 	sendMessage msg

newGroupMessage :: Task Void
newGroupMessage = getCurrentUser
	>>= \me ->		getMyGroups
	>>= \groups ->	case groups of
		[]	=	showMessage "No groups" "You are not a member of any group" Void
		_	=	enterChoice "Choose group" "Select group" groups
			>>= \group ->	writeMessage me "" group.members [] []
			>>= \msg ->		sendMessage msg
	
sendMessage :: Message -> Task Void
sendMessage msg = allProc [who @>> spawnProcess who True True
					((readMessage msg <<@ Subject ("Message from "+++toString (fromHtmlDisplay msg.Message.sender)+++": "+++msg.Message.subject)) <<@ msg.Message.priority) \\ who <- (msg.Message.recipients ++ if(isJust msg.cc) (fromJust msg.cc) [])] Closed
					>>| showMessageAbout "Message sent" "The following message has been sent:" msg >>| return Void

writeMessage :: User String [User] [User] [Message] -> Task Message
writeMessage me subj recipients cc thread = updateInformation "Compose" "Enter your message" {Message | (mkMsg me) & subject = subj, recipients = recipients, cc = if(isEmpty cc) Nothing (Just cc), previousMessages = (HtmlDisplay thread)}	

readMessage :: Message -> Task Void
readMessage msg=:{Message | previousMessages, subject} 
	= showMessageAboutA subject "You received a message" [ButtonAction (ActionLabel "Reply",Always), 
		ButtonAction (ActionLabel "Reply All",Always), ButtonAction (ActionLabel "Forward",Always), ButtonAction (ActionLabel "Delete", Always), ButtonAction (ActionLabel "Archive & Close",Always)] msg
	>>= \act -> case act of
		(ActionLabel "Reply",_)
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: "+++msg.Message.subject) [(fromHtmlDisplay msg.sender)] []  [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
			>>= \msg -> sendMessage msg
		(ActionLabel "Reply All",_)
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: "+++msg.Message.subject) [(fromHtmlDisplay msg.sender):[u \\ u <- msg.recipients | u <> me]] (if(isJust msg.cc) (fromJust msg.cc) []) [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
			>>= \msg -> sendMessage msg
		(ActionLabel "Forward",_)
			= 			getCurrentUser 
			>>= \me -> 	writeMessage me ("Fw: "+++msg.Message.subject) [] [] [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
			>>= \msg -> sendMessage msg
		(ActionLabel "Archive & Close",_) 
			=			dbCreateItem msg
			>>| 		showMessage "Archived" "Message stored in archive" Void
		(ActionLabel "Delete",_)
			=			dbDeleteItem (getItemId msg)
			>>|			showMessage "Deleted" "Message deleted" Void

getMyMessages :: Task [Message]
getMyMessages
	=	getContextWorker
	>>= \user ->
		dbReadAll
	>>= transform (filter (isRecipientOrCc user))
where
	isRecipientOrCc user msg = isMember user msg.Message.recipients || case msg.Message.cc of Just cc = isMember user cc; _ = False  

