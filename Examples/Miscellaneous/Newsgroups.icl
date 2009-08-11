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
		>>= \(me,myname) ->		getToNames
		>>= \tos ->				updateInformation "Type your message ..."
									(initMsg (foldl (\s1 s2 -> s1 +++ "; " +++ s2) "" (map snd tos)) myname "" "")
		>>= \msg ->				myAndTasks [Text "Mail send to:"] 
										[ ("For: " <+++ toname <+++ "; Subject: " <+++ msg.subject
										, MailAndReply msg (me,myname) (to,toname))
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
	>>= \(me,myname) ->		getToName
	>>= \(to,toname) ->		updateInformation "Type your message ..." (initMsg toname myname "" "")
	>>= \msg ->				(showMessageAbout "" msg) -&&- (to @: (msg.subject, showMessageAbout "" msg)) >>| return Void

initMsg to for subject msg 
= {to = to, mailFrom = for, subject = subject , message = Note msg}

addNewsGroup :: (Task Void)
addNewsGroup	= cancel addNewsGroup` 
where
	addNewsGroup`
	=						readNewsGroups
		>>= \groups ->		enterInformation (showCurrentNames groups ++ [Text "Enter new news group name to add:",BrTag []])
		>>= \newName ->		readNewsGroups
		>>= \groups ->		writeNewsGroups (removeDup (sort [newName:groups])) 
		>>= \groups ->		chooseTask (showCurrentNames groups ++ [Text "Do you want to add more ?"])
								[("Yes", addNewsGroup`)
								,("No",  return Void)
								]

showNewsGroups :: (Task Void)
showNewsGroups
=						readNewsGroups
	>>=	\groups	->		showCurrentNames groups ?>> ok 

showCurrentNames []		= [ Text "No names in catalogue yet !", BrTag [],BrTag []] 
showCurrentNames names	= [ Text "Current names in catalogue:", BrTag [],BrTag []
						  : visualizeAsHtmlDisplay (Note (foldr (\s1 s2 -> s1 +++ "\n" +++ s2) "" names))
						  ]

subscribeNewsGroup :: (Task Void)
subscribeNewsGroup
=						getCurrentUser
	>>= \(me,name) ->	readNewsGroups 
	>>= 				subscribe me name
where
	subscribe me myname []
	=						[Text "No newsgroups in catalogue yet:", BrTag [],BrTag []] ?>> ok 
	subscribe me myname groups
	=						requestChoice "Choose a group:" groups
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
	=						chooseTask "Browse through messagelist..."
								[ ("Previous " <+++ nmessage, readMoreNews (~nmessage))
								, ("Next " <+++ nmessage, 	  readMoreNews nmessage)
								]
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
	= 	[ Text ("Message : " <+++ i), BrTag []
		, Text ("From    : " <+++ name) , BrTag [], HrTag []
		, Text message ] ?>> ok
		

	commitItem :: String -> Task Void
	commitItem  group
	=								getCurrentUser
		>>= \(me,name) ->      		commit me name group
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
							?>> getToName
		>>= \(id,name) ->	let newnames = [(id,name):names] in
							chooseTask (showCurrentNames (map snd newnames) ++ [BrTag [], Text "More names to add? ", BrTag []])
								[ ("Yes", getToNames` newnames)
								, ("No",  return    newnames)
								]

getToName ::  (Task (Int,String))
getToName 
= 						getUsers
	>>= \users ->		chooseTask [Text "Select user to mail a message to: "]
							[(name, return (userId,name)) \\ (userId,name) <- users]


cancel :: (Task a) -> Task a | iTask a
cancel task = task -||- cancelTask <<@ TTVertical
where
	cancelTask = [HrTag []] ?>> button "Cancel Task" defaultValue


orTasks2 :: [HtmlTag] [LabeledTask a] -> Task a | iTask a
orTasks2 msg tasks = parallel "orTasks2"  (\list -> length list >= 1) hd undef [t <<@ l \\(l,t) <- tasks] <<@ (TTSplit msg)

myAndTasks msg tasks =	parallel "andTask" (\_ -> False) undef hd [t <<@ l \\(l,t) <- tasks] <<@ (TTSplit msg)

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


