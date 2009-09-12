implementation module Newsgroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create new newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news
// (c) mjp 2007 

import StdList, StdOrdList, StdTuple, StdMisc, GenBimap
import iTasks
import CommonDomain

:: NewsGroupNames:== [GroupName]				// list of newsgroup names
:: GroupName	:== String						// Name of the newsgroup
:: NewsGroup	:== [NewsItem]					// News stored in a news group
:: NewsItem		:== (Subscriber,Name,Message)	// id, name, and message of the publisher
:: Subscriber	:== Int							// the id of the publisher
:: Name			:== String						// the login name of the publisher
:: Message		:== String						// the message
:: Subscriptions:== [Subscription]				// newsgroup subscriptions of user
:: Subscription	:== (GroupName,Index)			// last message read in corresponding group
:: Index		:== Int							// 0 <= index < length newsgroup 

:: EMail	=	{ to 		:: !String
				, mailFrom 	:: !String
				, subject 	:: !String
				, message	:: !Note
				}

derive gPrint		EMail, EMail2
derive gParse		EMail, EMail2	
derive gVisualize	EMail, EMail2	
derive gUpdate		EMail, EMail2

nmessage = 2

newsgroupsExample :: [Workflow]
newsgroupsExample
=	[
	{	name		= "Examples/Miscellaneous/Newsgroups/Groups/Add"
	,	label		= "Add news group"
	,	roles		= []
	,	mainTask	= addNewsGroup
	},
	{	name		= "Examples/Miscellaneous/Newsgroups/Groups/Show"
	,	label		= "Show news groups"
	,	roles		= []
	,	mainTask	= showNewsGroups
	},
	{	name		= "Examples/Miscellaneous/Newsgroups/Groups/Subscribe"
	,	label		= "Subscribe to news group"
	,	roles		= []
	,	mainTask	= subscribeNewsGroup
	},
	{	name		= "Examples/Miscellaneous/Newsgroups/Mail/Simple"
	,	label		= "internal email"
	,	roles		= []
	,	mainTask	= internalEmail2
	},
	{	name		= "Examples/Miscellaneous/Newsgroups/Mail/With answers"
	,	label		= "internal emails with answers"
	,	roles		= []
	,	mainTask	= internalEmailResponse
	}
	]


:: EMail2	=	{ to` 		:: !UserId
				, subject` 	:: !String
				, message`	:: !Note
				}

internalEmail2 :: (Task Void)
internalEmail2
=							enterInformation "Type your email message ..."
	>>= \msg ->				msg.to` @: (msg.EMail2.subject`, showMessageAbout "You have received the following message:" msg)
	>>|						showMessage "Mail has been read."


internalEmailResponse :: (Task Void)
internalEmailResponse = cancel internalEmailResponse`
where
	internalEmailResponse`
	=							getCurrentUser
		>>= \me ->				getToNames
		>>= \tos ->				updateInformation "Type your message ..."
									(initMsg (foldl (\s1 s2 -> s1 +++ "; " +++ s2) "" (map snd tos)) me.User.displayName "" "")
		>>= \msg ->				myAndTasks [Text "Mail send to:"] 
										[ ("For: " <+++ toname <+++ "; Subject: " <+++ msg.subject
										, MailAndReply msg (me.User.userId,me.User.displayName) (to,toname))
										\\ (to,toname) <- tos
										]  
	where
		MailAndReply msg (me,myname) (to,toname)
		=						to @: 	( msg.subject
										, enterInformationAbout "Please draft a reply to the following message:" msg
										)
			>>= \(Note reply)
						->		me @: ( "Reply from: " <+++ toname <+++ "; Subject: " <+++ msg.subject
									  , showMessageAbout "" (initMsg myname toname ("RE: " <+++ msg.subject) reply)
									  )

	
internalEmail :: (Task Void)
internalEmail
=							getCurrentUser
	>>= \me ->				getToName
	>>= \(to,toname) ->		updateInformation "Type your message ..." (initMsg toname me.User.displayName "" "")
	>>= \msg ->				(showMessageAbout "" msg) -&&- (to @: (msg.subject, showMessageAbout "" msg)) >>| return Void

initMsg to for subject msg 
= {to = to, mailFrom = for, subject = subject , message = Note msg}

showCurrentGroups :: NewsGroupNames -> Task Void
showCurrentGroups groups = showStickyMessageAbout "Current groups:" groups

addNewsGroup :: (Task Void)
addNewsGroup	= cancel addNewsGroup` 
where
	addNewsGroup`
	=						readNewsGroups
		>>= \groups ->		showCurrentGroups groups ||- enterInformation "Enter new news group name to add:"
		>>= \newName ->		readNewsGroups
		>>= \groups ->		writeNewsGroups (removeDup (sort [newName:groups])) 
		>>= \groups ->		showCurrentGroups groups  ||- requestConfirmation "Do you want to add more?"
		>>= \yn ->			if yn addNewsGroup` (return Void)
							
showNewsGroups :: (Task Void)
showNewsGroups
=						readNewsGroups
	>>=	\groups	->		if (length groups == 0)
							(showMessage "No names in catalogue yet !")
							(showMessageAbout "Current names in catalogue: " groups)

subscribeNewsGroup :: (Task Void)
subscribeNewsGroup
=					getCurrentUser
	>>= \user ->	readNewsGroups 
	>>= 			subscribe user.User.userId user.User.displayName
where
	subscribe me myname []
	=						showMessage "No newsgroups in catalogue yet:"
	subscribe me myname groups
	=						enterChoice "Choose a group:" groups
		>>= \group ->		addSubscription me (group,0)
		>>|					spawnProcess me True (readNews me group 0 <<@ group <+++ " news group subscription")
		>>|					return Void



readNews :: Int String Int -> Task Void
readNews me group index	
=			orTasks2 [Text ("Welcome to newsgroup " +++ group)]
							 [("Read next news items from newsgroup " <+++ group, readMore)
							 ,("Commit new message to newsgroup " <+++ group,	  commitItem group >>| return index)
							 ,("Unsubscribe from newsgroup " <+++ group,		  unsubscribe) 
							 ,("Message list of newsgroup " <+++ group, 		  messageList index >>| return index)  
							 ]
		>>= \index -> if (index >= 0)
						(spawnProcess me True (readNews me group index <<@ group <+++ " news group subscription" ) >>| return Void) // CODE GENERATION BUG WHEN REPLACE BY >>| 
						(return Void)
where
	unsubscribe
	=						requestConfirmation "Do you realy want to unsubscribe ?"
								>>= \yn -> return (if yn -1 index) 

	readMore 
	=						(enterChoice "Browse through messagelist..."
								[ readMoreNews (~nmessage) <<@ ("Previous " <+++ nmessage)
								, readMoreNews nmessage <<@ ("Next " <+++ nmessage)
								]
							>>= \task -> task)
	where
		readMoreNews offset
		=					readIndex me group
			>>= \index ->	readNewsGroup group
			>>= \news ->	readNextNewsItems index offset (length news)
		where
			readNextNewsItems  index offset length
			# nix = index + offset
			# nix = if (nix < 0) 0 (if (length <= nix) index nix)
			= addSubscription me (group,nix) >>| return nix				 

	messageList index
	= 						readNewsGroup group 
		>>= \newsItems  ->	allTasks [show i newsItem <<@ ("Message " <+++ i) \\ newsItem <- newsItems%(index,index+nmessage-1) & i <- [index..]]
		>>|					showMessage "Refresh list"

	show :: Int NewsItem -> Task Void
	show i (who, name, message) 
	= 	showMessageAbout [Text ("Message: " <+++ i), BrTag [], Text ("From: " <+++ name)] message
		

	commitItem :: String -> Task Void
	commitItem  group
	=								getCurrentUser
		>>= \user ->      			commit user.User.userId user.User.displayName group
	where
		commit me name group
		=							enterInformation [Text "Type your message ..."] 
		 >>= \(Note val) -> 		readNewsGroup  group 
		 >>= \news ->				writeNewsGroup group (news ++ [(me,name,val)]) 
		 >>|						showMessage [Text "Message commited to news group ",BTag [] [Text group], BrTag [],BrTag []] 

			
getToNames = getToNames` []
where
	getToNames` names 	
	=						showCurrentNames (map snd names)
							||- getToName
		>>= \(id,name) ->	let newnames = [(id,name):names] in
								showCurrentNames (map snd newnames)
								||- requestConfirmation "Add more names?"
								>>= \yn ->
									if yn (getToNames` newnames) (return newnames)


showCurrentNames :: [String] -> Task Void
showCurrentNames names = showStickyMessageAbout "Current names:" names

getToName ::  (Task (Int,String))
getToName 
= 						getUsers
	>>= \users ->		enterChoice "Select user to mail a message to: " users
	>>= \user ->		return (user.User.userId,user.User.displayName)


cancel :: (Task a) -> Task a | iTask a
cancel task = task -||- (showMessage "Cancel this task" >>| getDefaultValue) <<@ TTVertical

orTasks2 :: [HtmlTag] [LabeledTask a] -> Task a | iTask a
orTasks2 msg tasks = parallel "orTasks2"  (\list -> length list >= 1) hd undef [t <<@ l \\(l,t) <- tasks] 

myAndTasks msg tasks =	parallel "andTask" (\_ -> False) undef hd [t <<@ l \\(l,t) <- tasks]

// reading and writing of storages

newsGroupsId ::  (DBid NewsGroupNames)
newsGroupsId		=	mkDBid "newsGroups"

readerId :: Int -> (DBid Subscriptions)
readerId i			= 	mkDBid ("reader" <+++ i)

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


