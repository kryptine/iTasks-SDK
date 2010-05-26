implementation module Newsgroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create new newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news
// (c) mjp 2007 

import StdList, StdOrdList, StdTuple, StdMisc
import iTasks
import Text
import CommonDomain

derive gPrint		EMail, Reply, DisplayNews, Broadcast, ReplyHdr
derive gParse		EMail, Reply, DisplayNews, Broadcast, ReplyHdr
derive gVisualize	EMail, Reply, DisplayNews, Broadcast, ReplyHdr	
derive gUpdate		EMail, Reply, DisplayNews, Broadcast, ReplyHdr
derive gError		EMail, Reply, DisplayNews, Broadcast, ReplyHdr
derive gHint		EMail, Reply, DisplayNews, Broadcast, ReplyHdr

derive bimap		Maybe, (,)

newsgroupsExample :: [Workflow]
newsgroupsExample
=	[	workflow	 "Examples/Communication/Newsgroups" handleMenu
	,	workflow	 "Examples/Communication/Mail" internalEmail
	,	workflow	 "Examples/Communication/Broadcast" internalBroadcast
	,	workflow	 "Examples/Communication/Mail with confirmation" internalEmailConf
	,	workflow	 "Examples/Communication/Mail with forced reply" internalEmailReply
	,	workflow	 "Examples/Communication/Make appointment" mkAppointment
	,	workflow	 "Examples/Communication/Delegate Instruction" mkInstruction
	,	workflow	 "Examples/Communication/Chat with someone" chat
	]

derive gPrint		InstructionMsg
derive gParse		InstructionMsg
derive gVisualize	InstructionMsg
derive gUpdate		InstructionMsg
derive gError		InstructionMsg
derive gHint		InstructionMsg

:: InstructionMsg	=	{ worker		:: !UserName
						, title			:: !String
						, instruction	:: !Note
						, attachments	:: !(Maybe [Document])
						}

mkInstruction :: Task Void
mkInstruction
	= 			mkMsg 
	>>= \msg -> msg.InstructionMsg.worker @: ("Instructions regarding: "+++msg.InstructionMsg.title, showInstructionAbout msg.InstructionMsg.title msg.instruction msg.attachments)
	
	where
		mkMsg :: Task InstructionMsg
		mkMsg = enterInformation "Please write down your instructions"

// --- Appointment Example ---

// date fixing
derive gPrint		Appointment, Meeting, Attending
derive gParse		Appointment, Meeting, Attending
derive gVisualize	Appointment, Meeting, Attending	
derive gUpdate		Appointment, Meeting, Attending
derive gError		Appointment, Meeting, Attending
derive gHint		Appointment, Meeting, Attending

derive gMerge				Meeting, Appointment, Attending
derive gMakeSharedCopy		Meeting, Appointment, Attending
derive gMakeLocalCopy		Meeting, Appointment, Attending

:: Appointment		=	{ topic :: Note
						}
:: Meeting 			=	{ date		:: Date
						, from_		:: Time
						, till		:: Time
						}
:: Attending		=	No | Yes
:: MeetingDB		:== (Appointment,[(Meeting,[(UserName, Maybe Attending)])]) 

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
		= spawnProcess u True ("Meeting Request" @>> task n) >>| 	startup (n+1) dbid us data
	where
		task :: Int -> Task MeetingDB
		task n 
			= 		updateShared "Meeting requested" [ButtonAction (ActionOk,IfValid)] dbid [appointEditor]
				>>= switch 
		where
			switch   (ActionOk,_) = task n
			switch  (_,result)	  = return result

			appointEditor = editor {editorFrom = editorFrom, editorTo = editorTo}
			
			editorFrom (goal, props) 
				= HtmlDisplay (goal, [let (user,att) = attlist!!n in (meeting, attlist, user +++> "  can you attend ?", Editable att) 
									\\ (meeting,attlist) <- props])

			editorTo (HtmlDisplay (goal, props)) _ 
				= (goal, [(meeting,let (user,att) = attlist!!n in updateAt n (user,yn) attlist) \\ (meeting, attlist,_,Editable yn) <- props])

	meetingGoal	:: Task Appointment
	meetingGoal = enterInformation "Describe the topic of the meeting:"	

	defineParticipants :: Task [UserName]
	defineParticipants = enterInformation "Select participants:"

	defineOptions :: Task [Meeting]
	defineOptions = enterInformation "Define date and time options:"

// ====== CHAT =====================================================
derive gPrint 			Chat, ChatMessage, ChatView, ChatMessageView
derive gParse			Chat, ChatMessage, ChatView, ChatMessageView
derive gUpdate			Chat, ChatMessage, ChatView, ChatMessageView
derive gVisualize		Chat, ChatMessage, ChatView, ChatMessageView
derive gMakeLocalCopy 	Chat, ChatMessage, ChatView, ChatMessageView
derive gMakeSharedCopy	Chat, ChatMessage, ChatView, ChatMessageView
derive gMerge			Chat, ChatMessage, ChatView, ChatMessageView
derive gError			Chat, ChatMessage, ChatView, ChatMessageView
derive gHint			Chat, ChatMessage, ChatView, ChatMessageView

//Shared State	
:: Chat =
	{ initUser	:: UserName
	, users		:: [UserName]
	, messages	:: [ChatMessage]
	}
	
:: ChatMessage =
	{ who		:: UserName
	, when		:: DateTime
	, message	:: Note
	, replies	:: [ChatMessage]
	}

//Transformed View
:: ChatView = 
	{ users		:: HtmlDisplay [UserName]
	, messages	:: HtmlDisplay [ChatMessageView]
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
	>>= \friends -> createChatBox (toUserName me)
	>>= \chatbox ->	allTasks ([spawnProcess f True ("Chat Request" @>> (initiateChat chatbox f [un me:friends])) \\ f <- friends]
							++ [spawnProcess (un me) True ("Chat Request" @>> (initSession >>| chatSession chatbox (un me)))]) 						
where
	un me = toUserName me
	
	createChatBox :: UserName -> (Task (DBid Chat))
	createChatBox me = createDB {Chat | initUser = me, users = [], messages = []}

	selectFriends :: Task [UserName]
	selectFriends = enterInformation "Whom do you want to chat with?"
	
	initiateChat :: (DBid Chat) UserName [UserName] -> Task Void
	initiateChat chatbox friend friends
		=	requestConfirmation ("Do you want to initiate a chat with "+++printFriends+++"?")
		>>= \yes -> if yes
						(initSession >>| chatSession chatbox friend)
						(return Void)
	where
		printFriends = join ", " (map toString friends)

	initSession :: Task Void
	initSession = setMenus
		[ Menu "File" [ MenuItem "New Topic" ActionNew
					  , MenuItem "Add Users" ActionAddUser
					  , MenuItem "Quit"		 ActionQuit
					  ]
		]
	
	chatSession :: (DBid Chat) UserName -> Task Void
	chatSession chatbox user 
		= 			readDB chatbox
		>>= \chat -> writeDB chatbox {Chat | chat & users = chat.Chat.users++[user]}
		>>|	dynamicGroupAOnly [chatEditor chatbox user <<@ GBAlwaysFixed] (chatActions chatbox user)
	where
		chatActions :: (DBid Chat) UserName -> [GroupAction GOnlyAction Void Chat]
		chatActions chatbox user = [ GroupAction	ActionNew		(GOExtend [ignoreResult (newTopic chatbox user)]) 	GroupAlways
						 		   , GroupAction 	ActionQuit		GOStop												GroupAlways
						 		   , GroupAction	ActionAddUser	(GOExtend [ignoreResult (addUsers chatbox)])		(SharedPredicate chatbox (\(SharedValue chat) -> chat.Chat.initUser == user)) 
						 		   ]
						 		   	
	chatEditor :: (DBid Chat) UserName -> Task Void
	chatEditor chatbox user = ignoreResult (getCurrentDateTime >>= \dt -> updateShared "Chat" [] chatbox [mainEditor user dt])
	
	mainEditor :: UserName DateTime -> (View Chat)
	mainEditor user dt = editor {editorFrom = editorFrom user, editorTo = editorTo user dt}
	where
		editorFrom :: UserName Chat -> ChatView
		editorFrom user chat = {ChatView 
							   | users 		= HtmlDisplay chat.Chat.users
							   , messages 	= HtmlDisplay [(convertMessageToView user msg) \\ msg <- chat.Chat.messages]
							   }
		where
			convertMessageToView :: UserName ChatMessage -> ChatMessageView
			convertMessageToView user msg =
				{ ChatMessageView
				| info		= toString msg.ChatMessage.who+++" said at "+++toString msg.ChatMessage.when
				, message	= if(user == msg.ChatMessage.who) (VHEditable msg.ChatMessage.message) (VHHtmlDisplay msg.ChatMessage.message)
				, replies	= [convertMessageToView user reply \\ reply <- msg.ChatMessage.replies]
				, addReply	= Editable {FormButton | label = "Add reply", icon = "", state = NotPressed}
				}
		
		editorTo :: UserName DateTime ChatView Chat -> Chat
		editorTo user dt view chat = {Chat
								  | chat
								  & messages 	= [convertViewToMessage user dt vmsg omsg \\ vmsg <- (fromHtmlDisplay view.ChatView.messages) & omsg <- chat.Chat.messages]
								  }
		where
			convertViewToMessage :: UserName DateTime ChatMessageView ChatMessage -> ChatMessage
			convertViewToMessage user dt vmsg omsg =
				{ ChatMessage
				| who		= omsg.ChatMessage.who
				, when		= omsg.ChatMessage.when
				, message	= fromVizHint vmsg.ChatMessageView.message
				, replies	= [convertViewToMessage user dt vreply oreply \\ vreply <- vmsg.ChatMessageView.replies & oreply <- omsg.ChatMessage.replies ] ++ addReply (fromEditable vmsg.addReply) user dt
				}
			
			addReply :: FormButton UserName DateTime -> [ChatMessage]
			addReply button user dt 
				= case button.state of
					Pressed
						= [{ChatMessage | who = user, when = dt, message = Note "", replies = []}]
					NotPressed
						= []
				
			fromVizHint (VHEditable x) 		= x
			fromVizHint (VHHtmlDisplay x) 	= x
			fromVizHint (VHHidden x) 		= x
	
	newTopic :: (DBid Chat) UserName -> Task Void
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
	
	addUsers :: (DBid Chat) -> Task Void
	addUsers chatbox
		= 			 	enterInformation "Select users to add to the chat"	
		>>= \users -> 	readDB chatbox
		>>= \chat ->	allTasks ([spawnProcess u True ("Chat Request" @>> (initiateChat chatbox u (chat.Chat.users++users))) \\ u <- users])
		>>| 		 	return Void
//===============================================

// mail handling, to be put in sepparate icl file

:: EMail	=	{ to 			:: !UserName
				, cc			:: ![UserName]
				, subject 		:: !String
				, message		:: !Note
				, attachements	:: ![Document]
				}
:: ReplyHdr	=	{ replyFrom		:: !UserName
				, subject 		:: !String
				}
:: Reply =		{ reply			:: !Note
				, attachements	:: ![Document]
				}
:: Broadcast =	{ subject 		:: !String
				, message		:: !Note
				, attachements	:: ![Document]
				} 

internalBroadcast :: (Task Broadcast)
internalBroadcast
=									enterInformation "Type your broadcast message ..."
	>>= \msg ->						getCurrentUser
	>>= \me ->						getUsers
	>>= \users ->					broadcast me msg users
	>>|								return msg
where
	broadcast :: User Broadcast [User] -> Task Broadcast
	broadcast me msg [] 
		=			return msg
	broadcast me msg [u:us] 
		=			spawnProcess (UserName u.userName u.displayName)True
						(showMessageAbout ("You have received the following broadcast message from " <+++ me.displayName) msg <<@ msg.Broadcast.subject)
			>>|			broadcast me msg us 
				

internalEmail :: (Task EMail)
internalEmail
=									enterInformation "Type your email message ..."
	>>= \msg ->						getCurrentUser
	>>= \me ->						allProc [{user = who, task = spawnProcess who True (mailMess me msg <<@ msg.EMail.subject)} \\ who <- [msg.to:msg.cc]] Closed
	>>|								return msg

mailMess :: User EMail -> Task Void
mailMess me msg = showMessageAbout ("Mail from " <+++ me.displayName <+++ ":") msg 

internalEmailConf :: (Task EMail)
internalEmailConf
=						enterInformation "Type your email message ..."
	>>= \msg ->			getCurrentUser
	>>= \me ->			allProc [{user = who, task = (mailMess me msg <<@ msg.EMail.subject)} \\ who <- [msg.to:msg.cc]] Closed
	>>|					return msg

getUserName (UserName id name) = name
getUserId   (UserName id name) = id
	
internalEmailReply :: (Task (EMail,[Reply])) 
internalEmailReply
=					enterInformation "Type your email message ..."
	>>= \msg ->		getCurrentUser
	>>= \me ->		allProc [{user = who, task = mailMess2 me msg <<@ msg.EMail.subject} \\ who <- [msg.to:msg.cc]] Closed
	>>= \reply ->	showMessageAbout "The following replies have been commited:" reply
	>>|				return (msg,map snd reply)

mailMess2 :: User EMail -> Task (ReplyHdr, Reply)
mailMess2 me msg 
	= 	(showStickyMessageAbout ("Mail from " <+++ me.displayName <+++ ":") msg 
	  	||- updateInformation "The sender requested a reply..." 
   				(HtmlDisplay {replyFrom = toUserName me, subject = "Re: " +++ msg.EMail.subject}, {reply = Note "", attachements = [] }))
					<<@ msg.EMail.subject
		>>= \(HtmlDisplay hdr,reply) -> return (hdr,reply)

// newsgroup handling

:: NewsGroupNames	:== [GroupName]					// list of newsgroup names
:: GroupName		:== String						// Name of the newsgroup
:: NewsGroup		:== [NewsItem]					// News stored in a news group
:: NewsItem			:== (Subscriber,Message)		// id, name, and message of the publisher
:: Subscriber		:== UserName					// the id of the publisher
:: Name				:== String						// the login name of the publisher
:: Message			:== Note						// the message
:: Subscriptions	:== [Subscription]				// newsgroup subscriptions of user
:: Subscription		:== (GroupName,Index)			// last message read in corresponding group
:: Index			:== Int							// 0 <= index < length newsgroup 
:: DisplayNews	=	{ messageNr :: Int
					, postedBy	:: UserName
					, message	:: Message
					}

// news group manager

ActionShowGroups	:== ActionLabel "ShowGroups"
ActionSubscribe		:== ActionLabel "Subscribe"
ActionSubscribeTo	:== ActionLabel "SubscribeTo"
ActionCommit		:== ActionLabel "Commit"

initMenu :: NewsGroupNames -> Task Void
initMenu groups
	= setMenus
		[ Menu "File"	[ MenuItem "Add New Newsgroup..."	ActionNew
						, SubMenu  "Subscribe to"			[MenuItem group (ActionParam "SubscribeTo" group) \\ group <- groups]
						, MenuSeparator
						, MenuItem "Quit"					ActionQuit
						]
		, Menu "Help"	[ MenuItem "About"					ActionShowAbout 
						]
		]

actions groups
	=	map MenuAction	[ (ActionNew,		 Always)
						, (ActionQuit,		 Always)
						, (ActionShowAbout,	 Always)
						: [(ActionParam "SubscribeTo" group ,valid) \\ group <- groups]
						] 
where
	valid 			= Predicate (\_ -> lengthGroups > 0)
	lengthGroups 	= length groups

okCancel
	=	[ ButtonAction	(ActionCancel,	Always)
		, ButtonAction	(ActionOk,	IfValid)
		]

handleMenu :: Task Void
handleMenu 
	=					readNewsGroups
		>>= \groups -> 	getCurrentUser
		>>= \me ->		initMenu groups >>| doMenu (toUserName me) groups
where
	doMenu me groups
		=						showMessageA "Newsgroup reader, select from menu..." (actions groups)
			>>= switch
	where
		switch ActionCancel 					= 												doMenu me groups
		switch ActionNew 						= addNewsGroup 								>>| handleMenu
		switch (ActionParam "SubscribeTo" group)= subscribeProcess me group					>>| doMenu me groups
		switch ActionShowAbout 					= showMessage "Newsgroup Reader vrs. 2.0" 	>>| doMenu me groups
		switch 	_ 								= return Void

addNewsGroup :: (Task Void)
addNewsGroup	
	=						readNewsGroups
		>>= \groups ->		enterInformationAboutA "Enter new news group name to add:"  okCancel groups 
		>>= switch
where
	switch 	(ActionCancel,_) = return Void
	switch 	(_,newName) 	
		= 					readNewsGroups
		>>= \groups ->		writeNewsGroups (removeDup (sort [newName:groups])) 
		>>= \groups ->		requestConfirmationAbout "Do you want to add more?" groups 
		>>= \yn ->			if yn addNewsGroup (return Void)
	
subscribeProcess me group = spawnProcess me True (readNews 1 me group 0 <<@ group <+++ " newsgroup reader")

// news group reader

ActionRefresh :== ActionLabel "Refresh"

readMenu :: Task Void
readMenu 
	= setMenus
		[ Menu "Menu"	[ SubMenu  "Show"	[MenuItem (i +++> " messages") (ActionParam "nmessage" (toString i)) \\ i <- [1,5,10,30,50]]
						, MenuItem "Quit"	ActionQuit
						]
		]

readactions nmessage index nmsg
	= map ButtonAction
		[ (ActionPrevious, 	Predicate (\_ -> (index > 0)))
		, (ActionRefresh, 	Always)
		, (ActionNext, 		Predicate (\_ -> (index + nmessage < nmsg)))
		, (ActionCommit, 	Always)
		, (ActionQuit, 		Always)
		] ++
		[MenuAction (ActionParam "nmessage" (toString i), Always) \\ i <- [1,5,10,30,50]]

readNews :: Int UserName String Int -> Task Void
readNews nmessage me group index
	= 	readMenu >>| readNews` nmessage me group index
where
	readNews` nmessage me group index	
	=						readNewsGroup group 
		>>= \newsItems ->	showMessageAboutA ("Newsgroup " <+++ group) (readactions nmessage index (length newsItems)) (messageList nmessage newsItems)
		>>= switch
	where
		switch ActionPrevious 	= readMoreNews (~nmessage) 	>>= readNews` nmessage me group
		switch ActionRefresh 	= 								readNews` nmessage me group index
		switch ActionNext 		= readMoreNews nmessage 	>>= readNews` nmessage me group
		switch ActionCommit 	= commitItem group 			>>| readNews` nmessage me group index
		switch (ActionParam _ n)= 								readNews (toInt n) me group index
		switch _ 				= return Void

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
			=							enterInformationA "Type your message ..." okCancel
			 	>>= switch
			where
				switch (ActionCancel,_) = return Void
				switch (_,note)
					=					readNewsGroup  group 
						>>= \news ->	writeNewsGroup group (news ++ [(toUserName me,note)]) 
			 			>>|				showMessage ("Message commited to newsgroup " <+++ group) 
	
		messageList nmessage newsItems
		= 	[show i newsItem \\ newsItem <- newsItems%(index,index+nmessage-1) & i <- [index..]]
	
		show i (who, message) 
		= 	{messageNr = i, postedBy = who, message = message} 
	
// reading and writing of storages

newsGroupsId ::  (DBid NewsGroupNames)
newsGroupsId		=	mkDBid "newsGroups"

readerId :: UserName -> (DBid Subscriptions)
readerId name		= 	mkDBid ("Reader-" <+++ getUserId name)

groupNameId :: String -> (DBid NewsGroup)
groupNameId name	=	mkDBid ("NewsGroup-" +++ name)

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

