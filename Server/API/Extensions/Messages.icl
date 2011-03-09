implementation module Messages

import iTasks
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
	databaseId				= sharedStore "Messages"
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
	>>= \res -> case res of
		(ActionOpen,Just message)		= manageMessage message	>>|	return False
		(Action "new-msg" _,_) 			= newMessage			>>|	return False
		(Action "new-group-msg" _,_)	= newGroupMessage		>>| return False
		(ActionQuit,_)					= 							return True
	) <! id >>| stop
where
	overview :: [Message] -> Task (Action,Maybe Message)
	overview []		= getDefaultValue >>= showMessageA ("My messages","You have no messages") [aNew,aNewGroup,aQuit] >>= transform (appSnd Just)
	overview msgs	= enterChoiceA ("My messages","Your messages:") id [aOpen,aNew,aNewGroup,aQuit] msgs
	
	aOpen		= (ActionOpen,ifvalid)
	aNew		= (Action "new-msg" "New message", always)
	aNewGroup	= (Action "new-group-msg" "New group message", always)
	aQuit		= (ActionQuit,always)

manageMessage :: Message -> Task Bool
manageMessage msg=:{Message |subject} 
	= 	showMessageAboutA (subject,"You received a message") id [aClose,aReply,aReplyAll,aForward,aDelete] msg
	>>= \act -> case act of
		(ActionClose,_) 
			= return False
		(Action "reply" _,message)
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: " +++ msg.Message.subject) [(fromDisplay msg.sender)] (Just msg)
			>>= \msg -> sendMessage msg
			>>| return True
		(Action "reply-all" _,_)
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: " +++ msg.Message.subject) [(fromDisplay msg.sender):[u \\ u <- msg.recipients | u <> me]] (Just msg)
			>>= \msg -> sendMessage msg
			>>| return True
		(Action "forward" _,_)
			= 			getCurrentUser 
			>>= \me -> 	writeMessage me ("Fw: " +++ msg.Message.subject) [] (Just msg)
			>>= \msg -> sendMessage msg
			>>| return False
		(ActionDelete,msg)
			=			dbDeleteItem (getItemId msg)
			>>|			showMessage ("Deleted","Message deleted") False	
where
	aReply		= (Action "reply" "Reply",always)
	aReplyAll	= (Action "reply-all" "Reply All",always)
	aForward	= (Action "forward" "Forward",always)
	aDelete		= (ActionDelete, always)
	aClose		= (ActionClose, always)

newMessage :: Task Void
newMessage
	=				getCurrentUser
	>>= \me -> 		writeMessage me "" [] Nothing
	>>= \msg -> 	sendMessage msg

newGroupMessage :: Task Void
newGroupMessage = getCurrentUser
	>>= \me ->		getMyGroups
	>>= \groups ->	case groups of
		[]	=	showMessage ("No groups","You are not a member of any group") Void
		_	=	enterChoice ("Choose group","Select group") groups
			>>= \group ->	writeMessage me "" group.members Nothing
			>>= \msg ->		sendMessage msg
	
sendMessage :: Message -> Task Void
sendMessage msg
	=	dbCreateItem msg
	>>= \msg -> case msg.needsReply of
			False	= allTasks [spawnProcess True True (notifyTask rcp msg) \\ rcp <- msg.Message.recipients] >>| stop
			True	= spawnProcess True True (awaitReplies msg) >>| stop
	>>| showMessageAbout ("Message sent","The following message has been sent:") msg
	>>| stop
where
	notifyTask user msg =
		user @>>
		subject msg @>>
		msg.Message.priority @>>
		(manageMessage msg)

	awaitReplies msg =
		Title ("Waiting for reply on " +++ msg.Message.subject) @>>
		case msg.Message.recipients of
			[recipient]	= assign recipient (askReplyTask recipient msg) >>= \answer -> notifyNoReplies [recipient] [answer]
			recipients	= allProc [askReplyTask rcp msg \\ rcp <- recipients] Closed >>=  notifyNoReplies recipients
	
	askReplyTask user msg =
		user @>>
		subject msg @>>
		msg.Message.priority @>>
		(showStickyMessage ("Reply requested","The sender would like to receive a reply to this message.") False
		 ||-
		 manageMessage msg
		 ) 
	subject msg
		= Title ("Message from " +++ toString (fromDisplay msg.Message.sender)+++ ": "+++msg.Message.subject)
	
	notifyNoReplies recipients answers
		= case [rcp \\ rcp <- recipients & ans <- answers | not ans] of
			[]		= stop
			users	= showMessageAbout ("Reply request ignored","The following users ignored your request for a reply:") users >>| stop
			
writeMessage :: User String [User] (Maybe Message) -> Task Message
writeMessage sender subj recipients mbThread
	= updateInformation ("Compose","Enter your message")
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
