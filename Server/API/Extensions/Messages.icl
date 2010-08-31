implementation module Messages

import iTasks
import CommonDomain
import Groups

derive class iTask Message
derive bimap Maybe, (,)

:: Message =
	{ messageId			:: Hidden Int
	, subject			:: String
	, sender			:: Display User
	, recipients 		:: [User]
	, priority 			:: TaskPriority
	, needsReply		:: Bool	
	, message			:: Note
	, attachments		:: Maybe [Document]
	, thread 			:: Display [Message]
	}

instance DB Message
where
	databaseId				= mkDBId "Messages"
	getItemId m				= DBRef (fromHidden m.Message.messageId)
	setItemId (DBRef i) m 	= {Message|m & messageId = toHidden i}

mkMsg :: User -> Message
mkMsg me = { Message
			| messageId		= toHidden 0
			, sender 		= toDisplay me
			, subject		= "New message"
			, recipients 	= []
			, priority 		= NormalPriority
			, needsReply	= False
			, message		= Note ""
		  	, attachments	= Nothing
		   	, thread		= Display []
		   	}

manageMessages :: Task Void
manageMessages =
	(	getMyMessages
	>>= overview
	>>= \(action,message) -> case action of
		ActionOpen						= manageMessage message	>>|	return False
		ActionLabel "New message"		= newMessage			>>|	return False
		ActionLabel "New group message"	= newGroupMessage		>>| return False
		ActionQuit						= 							return True
	) <! id >>| stop
where
	overview :: [Message] -> Task (Action,Message)
	overview []		= getDefaultValue >>= showMessageA "My messages" "You have no messages" [aNew,aNewGroup,aQuit] 
	overview msgs	= enterChoiceA "My messages" "Your messages:" [aOpen,aNew,aNewGroup,aQuit] msgs
	
	aOpen		= ButtonAction (ActionOpen,IfValid)
	aNew		= ButtonAction (ActionLabel "New message", Always)
	aNewGroup	= ButtonAction (ActionLabel "New group message", Always)
	aQuit		= ButtonAction (ActionQuit,Always)

manageMessage :: Message -> Task Bool
manageMessage msg=:{Message |subject} 
	= 	showMessageAboutA subject "You received a message" [aClose,aReply,aReplyAll,aForward,aDelete] msg
	>>= \act -> case act of
		(ActionClose,_) 
			= return False
		(ActionLabel "Reply",message)
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: "+++msg.Message.subject) [(fromDisplay msg.sender)] (Just msg)
			>>= \msg -> sendMessage msg
			>>| return True
		(ActionLabel "Reply All",_)
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: "+++msg.Message.subject) [(fromDisplay msg.sender):[u \\ u <- msg.recipients | u <> me]] (Just msg)
			>>= \msg -> sendMessage msg
			>>| return True
		(ActionLabel "Forward",_)
			= 			getCurrentUser 
			>>= \me -> 	writeMessage me ("Fw: " +++ msg.Message.subject) [] (Just msg)
			>>= \msg -> sendMessage msg
			>>| return False
		(ActionLabel "Delete",msg)
			=			dbDeleteItem (getItemId msg)
			>>|			showMessage "Deleted" "Message deleted" False	
where
	aReply		= ButtonAction (ActionLabel "Reply",Always)
	aReplyAll	= ButtonAction (ActionLabel "Reply All",Always)
	aForward	= ButtonAction (ActionLabel "Forward",Always)
	aDelete		= ButtonAction (ActionLabel "Delete", Always)
	aClose		= ButtonAction (ActionClose, Always)

newMessage :: Task Void
newMessage
	=				getCurrentUser
	>>= \me -> 		writeMessage me "" [] Nothing
	>>= \msg -> 	sendMessage msg

newGroupMessage :: Task Void
newGroupMessage = getCurrentUser
	>>= \me ->		getMyGroups
	>>= \groups ->	case groups of
		[]	=	showMessage "No groups" "You are not a member of any group" Void
		_	=	enterChoice "Choose group" "Select group" groups
			>>= \group ->	writeMessage me "" group.members Nothing
			>>= \msg ->		sendMessage msg
	
sendMessage :: Message -> Task Void
sendMessage msg
	=	dbCreateItem msg
	>>= \msg -> case msg.needsReply of
			False	= allTasks [spawnProcess True True (notifyTask rcp msg) \\ rcp <- msg.Message.recipients] >>| stop
			True	= spawnProcess True True (awaitReplies msg) >>| stop
	>>| showMessageAbout "Message sent" "The following message has been sent:" msg
	>>| stop
where
	notifyTask user msg =
		user @>>
		subject msg @>>
		msg.Message.priority @>>
		(manageMessage msg)

	awaitReplies msg =
		Subject ("Waiting for reply on " +++ msg.Message.subject) @>>
		case msg.Message.recipients of
			[recipient]	= assign recipient (askReplyTask recipient msg) >>= \answer -> notifyNoReplies [recipient] [answer]
			recipients	= allProc [askReplyTask rcp msg \\ rcp <- recipients] Closed >>=  notifyNoReplies recipients
	
	askReplyTask user msg =
		user @>>
		subject msg @>>
		msg.Message.priority @>>
		(showStickyMessage "Reply requested" "The sender would like to receive a reply to this message." False
		 ||-
		 manageMessage msg
		 ) 
	subject msg
		= Subject ("Message from " +++ toString (fromDisplay msg.Message.sender)+++ ": "+++msg.Message.subject)
	
	notifyNoReplies recipients answers
		= case [rcp \\ rcp <- recipients & ans <- answers | not ans] of
			[]		= stop
			users	= showMessageAbout "Reply request ignored" "The following users ignored your request for a reply:" users >>| stop
			
writeMessage :: User String [User] (Maybe Message) -> Task Message
writeMessage sender subj recipients mbThread
	= updateInformation "Compose" "Enter your message"
		{Message | (mkMsg sender) & subject = subj, recipients = recipients,thread = updateThread mbThread}
where
	updateThread :: (Maybe Message) -> Display [Message] 
	updateThread Nothing	= Display []
	updateThread (Just msg)	= Display [{Message|msg & thread = Display []}:fromDisplay msg.Message.thread]
	
getMyMessages :: Task [Message]
getMyMessages
	=	getContextWorker
	>>= \user ->
		dbReadAll
	>>= transform (filter (isRecipient user))
where
	isRecipient user msg = isMember user msg.Message.recipients

getAllMessages ::Task [Message]
getAllMessages = dbReadAll
