implementation module Newsgroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create new newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news
// (c) mjp 2007 

import StdList, StdOrdList, StdTuple, StdMisc
import iTasks
import CommonDomain

derive gPrint		EMail, Reply, DisplayNews, Broadcast
derive gParse		EMail, Reply, DisplayNews, Broadcast
derive gVisualize	EMail, Reply, DisplayNews, Broadcast	
derive gUpdate		EMail, Reply, DisplayNews, Broadcast

derive bimap		Maybe, (,)

newsgroupsExample :: [Workflow]
newsgroupsExample
=	[	workflow	 "Examples/Communication/Newsgroups" handleMenu
	,	workflow	 "Examples/Communication/mail" internalEmail
	,	workflow	 "Examples/Communication/broadcast" internalBroadcast
	,	workflow	 "Examples/Communication/mail with confirmation" internalEmailConf
	,	workflow	 "Examples/Communication/mail with forced reply" internalEmailReply
	,	workflow	 "Examples/Communication/make appointment" mkAppointment
	,	workflow	 "Examples/Communication/Delegate Instruction" mkInstruction
	,	workflow	 "Examples/Communication/Chat with someone" chat
	]

derive gPrint		InstructionMsg
derive gParse		InstructionMsg
derive gVisualize	InstructionMsg
derive gUpdate		InstructionMsg

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

derive gMerge				Meeting, Appointment, UserName, Attending
derive gMakeSharedCopy		Meeting, Appointment, UserName, Attending
derive gMakeLocalCopy		Meeting, Appointment, UserName, Attending

:: Appointment		=	{ topic :: Note
						}
:: Meeting 			=	{ date		:: Date
						, from_		:: Time
						, till		:: Time
						}
:: Attending		=	Yes | No | Remark Note

:: MeetingDB		:== (Appointment,[(Meeting,[(Maybe Attending,UserName)])]) 

import StdDebug, GenPrint

//mkAppointment :: Task (Appointment,Meeting,[UserName])
mkAppointment 
	= 					meetingGoal 
		>>= \goal -> 	defineParticipants 
		>>= \users ->	defineOptions
		>>= \dates ->	let 	sharedData :: MeetingDB
								sharedData = (goal,[(date,[(Nothing, user)\\ user <- users])\\ date <- dates])
						in (createDB sharedData
								>>= \dbid -> startup dbid users sharedData)
		>>= showMessageAbout "Result"
where
	startup :: (DBid MeetingDB) [UserName] MeetingDB -> Task MeetingDB
	startup dbid [] data = return data
	startup dbid users data		
		= let d = (length (snd data)-1)
		  in 
			allProc [{user = u, task = ("Meeting Request" @>> task n d)} \\ u <- users & n <- [0..]] Open
			>>| readDB dbid
	where			
		task :: Int Int -> Task [MeetingDB]
		task uid len
			= allTasks [updateShared "Meeting request" [ButtonAction (ActionOk,IfValid)] dbid [appointEditor idx] \\ idx <- [0..len]]
				>>= \list -> return (map snd list)
		where
			
			appointEditor idx = editor {editorFrom = editorFrom idx, editorTo = editorTo idx}

			editorFrom :: Int MeetingDB -> (Static (Meeting,[(Maybe Attending,UserName)]),(Static String,(Maybe Attending)))
			editorFrom idx (goal, dates)
				# (date,attlist) = dates !! idx
				= let (att,user) = attlist !! uid in (Static (date,attlist),(Static(user +++> " can you attend?"),att))
			
			editorTo :: Int (Static (Meeting,[(Maybe Attending,UserName)]),(Static String,(Maybe Attending))) MeetingDB -> MeetingDB 				
			editorTo idx (info,(question,att)) (goal, dates)
				= let (meeting,attlist) = dates !! idx 
				  in (goal,updateAt idx (meeting, let (_,user) = attlist !! uid 
				  								  in updateAt uid (att,user) attlist) dates)

			//pred (Valid (_,props))	= and [let (att,user) = attlist!!n in isJust att \\ (_,attlist) <- props] // Lijkt deze niet te testen...
			//pred _					= False 

	meetingGoal	:: Task Appointment
	meetingGoal = enterInformation "Describe the topic of the meeting:"	

	defineParticipants :: Task [UserName]
	defineParticipants = enterInformation "Select participants:"

	defineOptions :: Task [Meeting]
	defineOptions = enterInformation "Define date and time options:"

/*
mkAppointment 
	= 					meetingGoal 
		>>= \goal -> 	defineParticipants 
		>>= \users ->	defineOptions
		>>= \dates ->	let 	sharedData :: (Appointment,[( Meeting,[(Maybe Attending,UserName)])])
								sharedData = (goal,[(date,[(Nothing, user)\\ user <- users])\\ date <- dates])
							in (createDB sharedData
								>>= \dbid -> startup 0 dbid users sharedData)
where
	startup n dbid [] _
		= return Void
	startup n dbid [u:us] data
		= spawnProcess u True ("Meeting Request" @>> task True n) >>| 	startup (n+1) dbid us data
	where
		task :: Bool Int -> Task (Appointment,[( Meeting,[(Maybe Attending,UserName)])])
		task notDone n 
			= 							if notDone
											(updateShared "Meeting requested" [ButtonAction (ActionOk,Predicate pred)] dbid [appointEditor])
											(updateShared "Meeting requested" [ButtonAction (ActionOk,Always)] dbid [idListener])
				>>= switch notDone
		where
			switch True  (ActionOk,_) = task False n
			switch False (_,result)	  = return result

			appointEditor = editor {editorFrom = editorFrom, editorTo = editorTo}
			
			editorFrom (goal, props) 
				= (Static goal, [let (att,user) = attlist!!n in (Static (meeting, attlist),(Static (user +++> "  can you attend ?"),att)) 
									\\ (meeting,attlist) <- props])

			editorTo (Static goal, props) _ 
				= (goal, [(meeting,let (att,user) = attlist!!n in updateAt n (yn,user) attlist) \\ (Static (meeting, attlist),(_,yn)) <- props])

			pred (Valid (_,props))	= and [let (att,user) = attlist!!n in isJust att \\ (_,attlist) <- props] // Lijkt deze niet te testen...
			pred _					= False 

	meetingGoal	:: Task Appointment
	meetingGoal = enterInformation "Describe the topic of the meeting:"	

	defineParticipants :: Task [UserName]
	defineParticipants = enterInformation "Select participants:"

	defineOptions :: Task [Meeting]
	defineOptions = enterInformation "Define date and time options:"
*/

// chat

chat 
	= 					getCurrentUser
		>>= \me ->		createChatBox
		>>= \chatBox ->	enterInformation "With whom do you want to Chat ?" 
		>>= \friend ->	friend @: ("Chat Request", chatSession chatBox friend) 
		>>= \yes ->		if yes (chatTask chatBox) (return Void)
where		
	createChatBox :: (Task (DBid [Note]))
	createChatBox = createDB []

	chatSession :: (DBid [Note]) UserName -> Task Bool
	chatSession chatBox friend 
		= 				requestConfirmation "Do you want to Chat with me ?"
			>>= \yes -> if yes 
							(spawnProcess friend True (chatTask chatBox) >>| return True)
							(return False)

	chatTask chatBox = updateShared "Chat" [] chatBox [chatEditor] >>| return Void

	chatEditor :: (View [Note])
	chatEditor = idEditor

// mail handling, to be put in sepparate icl file

:: EMail	=	{ to 			:: !UserName
				, cc			:: ![UserName]
				, subject 		:: !String
				, message		:: !Note
				, attachements	:: ![Document]
				}
:: Reply	=	{ reply			:: !Note
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
	>>= \me ->						allProc [{user = who, task = spawnProcess msg.to True (mailMess me msg) <<@ msg.EMail.subject} \\ who <- [msg.to:msg.cc]] Closed
	>>|								return msg

mailMess :: User EMail -> Task Void
mailMess me msg = showMessageAbout ("Mail from " <+++ me.displayName <+++ ":") msg 

internalEmailConf :: (Task EMail)
internalEmailConf
=									enterInformation "Type your email message ..."
	>>= \msg ->						getCurrentUser
	>>= \me ->						allProc [{user = who, task = mailMess me msg <<@ msg.EMail.subject} \\ who <- [msg.to:msg.cc]] Closed
	>>|								return msg

getUserName (UserName id name) = name
getUserId   (UserName id name) = id
	
internalEmailReply :: (Task (EMail,[Reply])) 
internalEmailReply
=									enterInformation "Type your email message ..."
	>>= \msg ->						getCurrentUser
	>>= \me ->						allProc [{user = who, task = mailMess2 me msg <<@ msg.EMail.subject} \\ who <- [msg.to:msg.cc]] Closed
	>>= \reply ->					return (msg,reply)

mailMess2 :: User EMail -> Task Reply
mailMess2 me msg = 	(showStickyMessageAbout ("Mail from " <+++ me.displayName <+++ ":") msg 
				  	||- 
			   		enterInformation "The sender requested a reply...")
					<<@ msg.EMail.subject

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

