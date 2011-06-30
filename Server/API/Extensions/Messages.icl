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
	databaseId				= sharedStore "Messages" []
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
		(Action "New message",_) 		= newMessage			>>|	return False
		(Action "New group message",_)	= newGroupMessage		>>| return False
		(ActionQuit,_)					= 							return True
	) <! id >>| return Void
where
	overview :: [Message] -> Task (Action,Maybe Message)
	overview []		= showInformation ("My messages","You have no messages") [] Void >>+ \_ -> UserActions [(aNew,Just (aNew,Nothing)),(aNewGroup,Just (aNewGroup,Nothing)),(aQuit,Just (aQuit,Nothing))]
	overview msgs	= enterChoice ("My messages","Your messages:") [] msgs >>+ \{modelValue,localValid} -> let mbM = if localValid (Just modelValue) Nothing in UserActions [(aOpen,maybe Nothing (\m -> Just (aOpen,Just m)) mbM),(aNew,Just (aNew,Nothing)),(aNewGroup,Just (aNewGroup,Nothing)),(aQuit,Just (aQuit,Nothing))]
	
	aOpen		= ActionOpen
	aNew		= Action "New message"
	aNewGroup	= Action "New group message"
	aQuit		= ActionQuit

manageMessage :: Message -> Task Bool
manageMessage msg=:{Message |subject} 
	= 	showInformation (subject,"You received a message") [About msg] Void >>+ (\_ -> UserActions [(aClose,Just aClose),(aReply,Just aReply),(aReplyAll,Just aReplyAll),(aForward,Just aForward),(aDelete,Just aDelete)])
	>>= \act -> case act of
		ActionClose
			= return False
		(Action "Reply")
			= 			get currentUser
			>>= \me	->	writeMessage me ("Re: " +++ msg.Message.subject) [(fromDisplay msg.sender)] (Just msg)
			>>= \msg -> sendMessage msg
			>>| return True
		(Action "Reply All")
			= 			get currentUser
			>>= \me	->	writeMessage me ("Re: " +++ msg.Message.subject) [(fromDisplay msg.sender):[u \\ u <- msg.recipients | u <> me]] (Just msg)
			>>= \msg -> sendMessage msg
			>>| return True
		(Action "Forward")
			= 			get currentUser 
			>>= \me -> 	writeMessage me ("Fw: " +++ msg.Message.subject) [] (Just msg)
			>>= \msg -> sendMessage msg
			>>| return False
		ActionDelete
			=			dbDeleteItem (getItemId msg)
			>>|			showInformation ("Deleted","Message deleted") [] False	
where
	aReply		= Action "Reply"
	aReplyAll	= Action "Reply All"
	aForward	= Action "Forward"
	aDelete		= ActionDelete
	aClose		= ActionClose

newMessage :: Task Void
newMessage
	=				get currentUser
	>>= \me -> 		writeMessage me "" [] Nothing
	>>= \msg -> 	sendMessage msg

newGroupMessage :: Task Void
newGroupMessage = get currentUser
	>>= \me ->		getMyGroups
	>>= \groups ->	case groups of
		[]	=	showInformation ("No groups","You are not a member of any group") [] Void
		_	=	enterChoice ("Choose group","Select group") [] groups
			>>= \group ->	writeMessage me "" group.members Nothing
			>>= \msg ->		sendMessage msg
	
sendMessage :: Message -> Task Void
sendMessage msg
	=	dbCreateItem msg
	>>= \msg -> case msg.needsReply of
			False	= allTasks [spawnProcess True {worker = rcp, priority = msg.Message.priority, deadline = Nothing, status = Active} (subject msg @>> manageMessage msg) \\ rcp <- msg.Message.recipients] >>| return Void
			True	= spawnProcess True initManagerProperties (awaitReplies msg) >>| return Void
	>>| showInformation ("Message sent","The following message has been sent:") [About msg] Void
where
	awaitReplies msg =
		Description ("Waiting for reply on " +++ msg.Message.subject) @>>
		case msg.Message.recipients of
			[recipient]	= recipient @: (askReplyTask recipient msg) >>= \answer -> notifyNoReplies [recipient] [answer]
			recipients	= allTasks [askReplyTask rcp msg \\ rcp <- recipients] >>=  notifyNoReplies recipients
	
	askReplyTask user msg =
		subject msg @>>
			((showInformation ("Reply requested","The sender would like to receive a reply to this message.") [] Void >>+ noActions`)
			 ||-
			 manageMessage msg
			 )

	subject msg
		= Description ("Message from " +++ toString (fromDisplay msg.Message.sender)+++ ": "+++msg.Message.subject)
	
	notifyNoReplies recipients answers
		= case [rcp \\ rcp <- recipients & ans <- answers | not ans] of
			[]		= return Void
			users	= showInformation ("Reply request ignored","The following users ignored your request for a reply:") [About users] Void

	noActions` :: (TermFunc a Void) | iTask a
	noActions` = noActions
			
writeMessage :: User String [User] (Maybe Message) -> Task Message
writeMessage sender subj recipients mbThread
	= updateInformation ("Compose","Enter your message") []
		{Message | (mkMsg sender) & subject = subj, recipients = recipients,thread = updateThread mbThread}
where
	updateThread :: (Maybe Message) -> Display [Message] 
	updateThread Nothing	= Display []
	updateThread (Just msg)	= Display [{Message|msg & thread = Display []}:fromDisplay msg.Message.thread]
	
getMyMessages :: Task [Message]
getMyMessages
	=	get currentUser
	>>= \user ->
		dbReadAll
	>>= transform (filter (isRecipient user))
where
	isRecipient user msg = isMember user msg.Message.recipients

getAllMessages ::Task [Message]
getAllMessages = dbReadAll
