implementation module Newsgroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create new newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news
// (c) mjp 2007 

import StdList, StdOrdList, StdTuple, StdMisc
import iTasks
import CommonDomain

derive gPrint		EMail, Reply, DisplayNews
derive gParse		EMail, Reply, DisplayNews
derive gVisualize	EMail, Reply, DisplayNews	
derive gUpdate		EMail, Reply, DisplayNews

derive bimap		Maybe, (,)

ifValid expr = Predicate (\val -> case val of
									Invalid -> False
									_ -> expr)

newsgroupsExample :: [Workflow]
newsgroupsExample
=	[	workflow	 "Examples/Communication/Newsgroups" handleMenu
	,	workflow	 "Examples/Communication/Mail with receive confirmation" internalEmail
	,	workflow	 "Examples/Communication/Mail with forced reply" internalEmailReply
	]

// mail handling, to be put in sepparate icl file

:: EMail	=	{ to 			:: !UserName
				, subject 		:: !String
				, message		:: !Note
				, attachements	:: ![Document]
				}
:: Reply	=	{ reply			:: !Note
				}

internalEmail :: (Task EMail)
internalEmail
=									enterInformation "Type your email message ..."
	>>= \msg ->						getCurrentUser
	>>= \me ->						msg.to @: (msg.EMail.subject, showMessageAbout ("You have received the following message from " <+++ me.displayName) msg)
	>>|								showMessage ("Your mail has been read by " <+++ getUserName msg.to)
	>>|								return msg

getUserName (UserName id name) = name
getUserId   (UserName id name) = id
	
internalEmailReply :: (Task (EMail,Reply)) // crashes ?? group
internalEmailReply
=									enterInformation "Type your email message ..."
	>>= \msg ->						getCurrentUser
	>>= \me ->						msg.to @: (msg.EMail.subject, (showStickyMessageAbout ("You have received the following message from " <+++ me.displayName) msg
																  ||- 
																   enterInformation "The sender requested a reply..."))
	>>= \reply->					showMessageAbout ("Your mail has been read by " <+++ getUserName msg.to) reply
	>>|								return (msg,reply)

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
		[ (ActionPrevious, 	Predicate (\_ -> (index - nmessage >= 0)))
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

