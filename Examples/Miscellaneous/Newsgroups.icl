implementation module Newsgroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create new newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news
// (c) mjp 2007 

import StdList, StdOrdList, StdTuple, StdMisc
import iTasks
import Text

derive class iTask	EMail, Reply, DisplayNews, Broadcast, ReplyHdr
derive bimap		Maybe, (,)

newsgroupsExample :: [Workflow]
newsgroupsExample
=	[	workflow	 "Examples/Communication/Newsgroups" "Set-up and read newsgroups" handleMenu
	,	workflow	 "Examples/Communication/Mail" "Send a message" internalEmail
	,	workflow	 "Examples/Communication/Broadcast" "Send a message to all users" internalBroadcast
	,	workflow	 "Examples/Communication/Mail with confirmation" "Send a message with read confirmation" internalEmailConf
	,	workflow	 "Examples/Communication/Mail with forced reply" "Send a message with a forced reply from the recipient" internalEmailReply
//	,	workflow	 "Examples/Communication/Make appointment"  mkAppointment
	,	workflow	 "Examples/Communication/Delegate Instruction" "Delegate work outside the system to a user" mkInstruction
	,	workflow	 "Examples/Communication/Chat with someone" "Set up a chat" chat
	]

derive class iTask	InstructionMsg

:: InstructionMsg	=	{ worker		:: !User
						, title			:: !String
						, instruction	:: !Note
						, attachments	:: !(Maybe [Document])
						}

mkInstruction :: Task Void
mkInstruction
	= 			mkMsg 
	>>= \msg -> msg.InstructionMsg.worker @: (Subject ("Instructions regarding: "+++msg.InstructionMsg.title) @>> showInstructionAbout msg.InstructionMsg.title msg.instruction msg.attachments >>| return Void)
	
	where
		mkMsg :: Task InstructionMsg
		mkMsg = enterInformation "Instructions" "Please write down your instructions"

// --- Appointment Example ---

// date fixing
derive class iTask	Appointment, Meeting, Attending
derive gMerge		Appointment, Meeting, Attending

:: Appointment		=	{ topic :: Note
						}
:: Meeting 			=	{ date		:: Date
						, from_		:: Time
						, till		:: Time
						}
:: Attending		=	No | Yes
:: MeetingDB		:== (Appointment,[(Meeting,[(User, Maybe Attending)])]) 

mkAppointment 
	= 					meetingGoal 
		>>= \goal -> 	defineOptions
		>>= \dates ->	defineParticipants 
		>>= \users ->	let 	sharedData :: MeetingDB
								sharedData = (goal,[(date,[(user, Nothing)\\ user <- users])\\ date <- dates])
							in (createDB sharedData
								>>= \dbid -> startup 0 dbid users sharedData)
where
	startup n dbid [] _
		= return Void
	startup n dbid [u:us] data
		= spawnProcess True True (u @>> Subject "Meeting Request" @>> task n) >>| 	startup (n+1) dbid us data
	where
		task :: Int -> Task MeetingDB
		task n 
			= 		updateShared "Request" "Meeting requested" [(ActionOk,ifvalid)] dbid [appointEditor]
				>>= switch 
		where
			switch  ((ActionOk,_),_)	= task n
			switch  (_,result)	  		= return result

			appointEditor = editor {editorFrom = editorFrom, editorTo = editorTo}
			
			editorFrom (goal, props) 
				= Display (goal, [let (user,att) = attlist!!n in (meeting, attlist, user +++> "  can you attend ?", Editable att) 
									\\ (meeting,attlist) <- props])

			editorTo (Display (goal, props)) _ 
				= (goal, [(meeting,let (user,att) = attlist!!n in updateAt n (user,yn) attlist) \\ (meeting, attlist,_,Editable yn) <- props])

	meetingGoal	:: Task Appointment
	meetingGoal = enterInformation "Topic" "Describe the topic of the meeting:"	

	defineParticipants :: Task [User]
	defineParticipants = enterInformation "Participants" "Select participants:"

	defineOptions :: Task [Meeting]
	defineOptions = enterInformation "Options" "Define date and time options:"

// ====== CHAT =====================================================
derive class iTask	Chat, ChatMessage, ChatView, ChatMessageView
derive gMerge		Chat, ChatMessage, ChatView, ChatMessageView

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
	
ActionAddUser :== Action "add-user" "Add User" 

chat
	=				getCurrentUser
	>>= \me ->		selectFriends
	>>= \friends -> createChatBox me
	>>= \chatbox ->	allTasks ([spawnProcess True True (f @>> Subject "Chat Request" @>> (initiateChat chatbox f [me:friends])) \\ f <- friends]
							++ [spawnProcess True True (me @>> Subject "Chat Request" @>> menus @>> chatSession chatbox (me))]) 						
where
	
	createChatBox :: User -> (Task (DBId Chat))
	createChatBox me = createDB {Chat | initUser = me, users = [], messages = []}

	selectFriends :: Task [User]
	selectFriends = enterInformation "Select friends" "Whom do you want to chat with?"
	
	initiateChat :: (DBId Chat) User [User] -> Task Void
	initiateChat chatbox friend friends
		=	requestConfirmation "Confirm" ("Do you want to initiate a chat with "+++printFriends+++"?")
		>>= \yes -> if yes
						(menus @>> chatSession chatbox friend)
						(return Void)
	where
		printFriends = join ", " (map toString friends)

	menus :: Menus
	menus =
		[ Menu "File" [ MenuItem (Action "new" "New Topic") Nothing
					  , MenuItem ActionAddUser				Nothing
					  , MenuItem ActionQuit					Nothing
					  ]
		]
	
	chatSession :: (DBId Chat) User -> Task Void
	chatSession chatbox user 
		= 			readDB chatbox
		>>= \chat -> writeDB chatbox {Chat | chat & users = chat.Chat.users++[user]}
		>>|	dynamicGroupAOnly [chatEditor chatbox user <<@ Fixed] chatActions chatActionsGenFunc
	where
		chatActions = [ (ActionNew, Always), (ActionQuit, Always)
					, (ActionAddUser, SharedPredicate chatbox (\(Just chat) -> chat.Chat.initUser == user))
					]
		chatActionsGenFunc (action, _) = case action of
			ActionNew		= GOExtend [newTopic chatbox user >>| stop]
			ActionQuit		= GOStop
			ActionAddUser	= GOExtend [addUsers chatbox >>| stop]
						 		   	
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
//===============================================

// mail handling, to be put in sepparate icl file

:: EMail	=	{ to 			:: !User
				, cc			:: !Maybe [User]
				, subject 		:: !String
				, message		:: !Note
				, attachements	:: !Maybe [Document]
				}
:: ReplyHdr	=	{ replyFrom		:: !User
				, subject 		:: !String
				}
:: Reply =		{ reply			:: !Note
				, attachements	:: !Maybe [Document]
				}
:: Broadcast =	{ subject 		:: !String
				, message		:: !Note
				, attachements	:: !Maybe [Document]
				} 

mbToList :: !(Maybe [a]) -> [a]
mbToList Nothing = []
mbToList (Just a) = a

listToMb :: ![a] -> (Maybe [a])
listToMb [] = Nothing
listToMb a = Just a

internalBroadcast :: (Task Broadcast)
internalBroadcast
=									enterInformation "Compose" "Type your broadcast message ..."
	>>= \msg ->						getCurrentUser
	>>= \me ->						getUsers
	>>= \users ->					broadcast me msg users
	>>|								return msg
where
	broadcast :: User Broadcast [User] -> Task Broadcast
	broadcast me msg [] 
		=			return msg
	broadcast me msg [u:us] 
		=			spawnProcess True True
						(showMessageAbout "Broadcast message" ("You have received the following broadcast message from " <+++ displayName me) msg <<@ Subject msg.Broadcast.subject <<@ u)
			>>|			broadcast me msg us 
				

internalEmail :: (Task EMail)
internalEmail
=									enterInformation "Compose" "Type your email message ..."
	>>= \msg ->						getCurrentUser
	>>= \me ->						allProc [who @>> (spawnProcess True True (mailMess me msg <<@ Subject msg.EMail.subject) <<@ who) \\ who <- [msg.to:mbToList msg.cc]] Closed
	>>|								return msg

mailMess :: User EMail -> Task Void
mailMess me msg = showMessageAbout "Mail" ("Mail from " <+++ displayName me <+++ ":") msg >>| return Void

internalEmailConf :: (Task EMail)
internalEmailConf
=						enterInformation "Compose" "Type your email message ..."
	>>= \msg ->			getCurrentUser
	>>= \me ->			allProc [who @>> (mailMess me msg <<@ Subject msg.EMail.subject) \\ who <- [msg.to:mbToList msg.cc]] Closed
	>>|					return msg
	
internalEmailReply :: (Task (EMail,[Reply])) 
internalEmailReply
=					enterInformation "Compose" "Type your email message ..."
	>>= \msg ->		getCurrentUser
	>>= \me ->		allProc [who @>> ( mailMess2 me msg <<@ Subject msg.EMail.subject) \\ who <- [msg.to:mbToList msg.cc]] Closed
	>>= \reply ->	showMessageAbout "Replies" "The following replies have been commited:" reply
	>>|				return (msg,map snd reply)

mailMess2 :: User EMail -> Task (ReplyHdr, Reply)
mailMess2 me msg 
	= 	(showStickyMessageAbout "New mail" ("Mail from " <+++ displayName me <+++ ":") msg 
	  	||- updateInformation "Reply" "The sender requested a reply..." 
   				(Display {replyFrom = me, subject = "Re: " +++ msg.EMail.subject}, {reply = Note "", attachements = Nothing }))
					<<@Subject  msg.EMail.subject
		>>= \(Display hdr,reply) -> return (hdr,reply)

// newsgroup handling

:: NewsGroupNames	:== [GroupName]					// list of newsgroup names
:: GroupName		:== String						// Name of the newsgroup
:: NewsGroup		:== [NewsItem]					// News stored in a news group
:: NewsItem			:== (Subscriber,Message)		// id, name, and message of the publisher
:: Subscriber		:== User						// the id of the publisher
:: Name				:== String						// the login name of the publisher
:: Message			:== Note						// the message
:: Subscriptions	:== [Subscription]				// newsgroup subscriptions of user
:: Subscription		:== (GroupName,Index)			// last message read in corresponding group
:: Index			:== Int							// 0 <= index < length newsgroup 
:: DisplayNews	=	{ messageNr :: Int
					, postedBy	:: User
					, message	:: Message
					}

// news group manager

ActionShowGroups	:== Action "show-groups" "ShowGroups"
ActionSubscribe		:== Action "subscribe" "Subscribe"
ActionSubscribeTo	:== Action "subscribe-to" "SubscribeTo"
ActionCommit		:== Action "commit" "Commit"

initMenu :: NewsGroupNames -> Menus
initMenu groups
	=
		[ Menu "File"	[ MenuItem (Action "Add New Newsgroup..." "new")	Nothing
						//, SubMenu  "Subscribe to"			[MenuItem group (ActionParam "subscribe-to" "Subscribe to" group) Nothing \\ group <- groups]
						, MenuSeparator
						, MenuItem ActionQuit								Nothing
						]
		, Menu "Help"	[ MenuItem ActionAbout								Nothing
						]
		]

actions groups
	=	[ (ActionNew,		always)
		, (ActionQuit,		always)
		, (ActionAbout,	 	always)
		//: [(ActionParam "subscribe-to" "Subscribe to" group ,valid) \\ group <- groups]
		] 
where
	valid 			= (\_ -> lengthGroups > 0)
	lengthGroups 	= length groups

okCancel
	=	[(ActionCancel,	always)
		,(ActionOk,	ifvalid)
		]

handleMenu :: Task Void
handleMenu 
	=					readNewsGroups
		>>= \groups -> 	getCurrentUser
		>>= \me ->		initMenu groups @>> doMenu me groups
where
	doMenu me groups
		=						showMessageA "Newsgroup reader" "Newsgroup reader, select from menu..." (actions groups) Void
			>>= switch
	where
		switch ((ActionCancel,_),_) 						= 															doMenu me groups
		switch ((ActionNew,_),_) 							= addNewsGroup 											>>| handleMenu
		//switch ((ActionParam "subscribe-to" _ group,_),_)	= subscribeProcess me group								>>| doMenu me groups
		switch ((ActionAbout,_),_) 							= showMessage "About" "Newsgroup Reader vrs. 2.0" Void 	>>| doMenu me groups
		switch 	_ 											= return Void

addNewsGroup :: (Task Void)
addNewsGroup	
	=						readNewsGroups
		>>= \groups ->		enterInformationAboutA "New group" "Enter new news group name to add:"  okCancel groups 
		>>= switch
where
	switch 	((ActionCancel,_),_) = return Void
	switch 	(_,newName) 	
		= 					readNewsGroups
		>>= \groups ->		writeNewsGroups (removeDup (sort [newName:groups])) 
		>>= \groups ->		requestConfirmationAbout "More groups?" "Do you want to add more?" groups 
		>>= \yn ->			if yn addNewsGroup (return Void)
	
subscribeProcess me group = spawnProcess True True (readNews 1 me group 0 <<@ Subject (group <+++ " newsgroup reader") <<@ me)

// news group reader

ActionRefresh :== Action "refresh" "Refresh"

readMenu :: Menus
readMenu =
	[ Menu "Menu"	[ //SubMenu  "Show"	[MenuItem (i +++> " messages") (ActionParam "nmessage" (i +++> " messages") (toString i)) Nothing \\ i <- [1,5,10,30,50]]
					 MenuItem ActionQuit Nothing
					]
	]

readactions nmessage index nmsg
	=	[ (ActionPrevious, 	(\_ -> (index > 0)))
		, (ActionRefresh, 	always)
		, (ActionNext, 		(\_ -> (index + nmessage < nmsg)))
		, (ActionCommit, 	always)
		, (ActionQuit, 		always)
		//: [(ActionParam "nmessage" (i +++> " messages") (toString i), always) \\ i <- [1,5,10,30,50]]
		]

readNews :: Int User String Int -> Task Void
readNews nmessage me group index
	= 	readMenu @>> readNews` nmessage me group index
where
	readNews` nmessage me group index	
	=						readNewsGroup group 
		>>= \newsItems ->	showMessageAboutA "Read news" ("Newsgroup " <+++ group) (readactions nmessage index (length newsItems)) (messageList nmessage newsItems)
		>>= switch
	where
		switch ((ActionPrevious,_),_) 	= readMoreNews (~nmessage) 	>>= readNews` nmessage me group
		switch ((ActionRefresh,_),_) 	= 								readNews` nmessage me group index
		switch ((ActionNext,_),_) 		= readMoreNews nmessage 	>>= readNews` nmessage me group
		switch ((ActionCommit,_),_) 	= commitItem group 			>>| readNews` nmessage me group index
		//switch ((ActionParam _ _ n,_),_)= 								readNews (toInt n) me group index
		switch _ 						= return Void

		readMoreNews offset
		=					readIndex me group
			>>= \index ->	readNewsGroup group
			>>= \news ->	readNextNewsItems index offset (length news)
		where
			readNextNewsItems index offset length
			# nix = index + offset
			# nix = if (nix < 0) 0 (if (length <= nix) index nix)
			= addSubscription me (group,nix) >>| return nix				 
	
		commitItem :: String -> Task Void
		commitItem  group
		=								getCurrentUser
			>>= \user ->      			commit user group
		where
			commit me group
			=							enterInformationA "Message" "Type your message ..." okCancel
			 	>>= switch
			where
				switch ((ActionCancel,_),_) = return Void
				switch (_,note)
					=					readNewsGroup  group 
						>>= \news ->	writeNewsGroup group (news ++ [(me,note)]) 
			 			>>|				showMessage "Committed" ("Message commited to newsgroup " <+++ group) Void
	
		messageList nmessage newsItems
		= 	[show i newsItem \\ newsItem <- newsItems%(index,index+nmessage-1) & i <- [index..]]
	
		show i (who, message) 
		= 	{messageNr = i, postedBy = who, message = message} 
	
// reading and writing of storages

newsGroupsId ::  (DBId NewsGroupNames)
newsGroupsId		=	mkDBId "newsGroups"

readerId :: User -> (DBId Subscriptions)
readerId user		= 	mkDBId ("Reader-" <+++ userName user)

groupNameId :: String -> (DBId NewsGroup)
groupNameId name	=	mkDBId ("NewsGroup-" +++ name)

readNewsGroups :: Task NewsGroupNames
readNewsGroups = readDB newsGroupsId

writeNewsGroups :: NewsGroupNames -> Task NewsGroupNames
writeNewsGroups newgroups = writeDB newsGroupsId newgroups

readSubscriptions :: Subscriber -> Task Subscriptions
readSubscriptions me = readDB (readerId me)

writeSubscriptions :: Subscriber Subscriptions -> Task Subscriptions
writeSubscriptions me subscriptions = writeDB (readerId me) subscriptions

addSubscription :: Subscriber Subscription -> Task Subscriptions
addSubscription me (groupname,index)
# index	= if (index < 0) 0 index
= 							readSubscriptions  me 
	>>= \subscriptions -> 	writeSubscriptions me [(groupname,index):[(group,index) \\ (group,index) <- subscriptions | group <> groupname]]

readIndex :: Subscriber GroupName -> Task Index
readIndex me groupname
= 							readSubscriptions me 
	>>= \subscriptions ->	return (hds [index \\ (group,index) <- subscriptions | group == groupname])
where
	hds [x:xs] = x
	hds [] = 0

readNewsGroup :: GroupName -> Task NewsGroup
readNewsGroup groupname = readDB (groupNameId groupname)

writeNewsGroup :: GroupName NewsGroup -> Task NewsGroup
writeNewsGroup groupname news = writeDB (groupNameId groupname) news

