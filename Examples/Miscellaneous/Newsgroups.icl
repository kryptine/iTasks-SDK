implementation module Newsgroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create new newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news
// (c) mjp 2007 

import StdList, StdOrdList, StdTuple, GenBimap
import iTasks, iDataTrivial, iDataFormlib, iDataWidgets


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

:: EMail	=	{ to 		:: !DisplayMode String
				, mailFrom 	:: !DisplayMode String
				, subject 	:: !String
				, message	:: !HtmlTextarea
				}

derive gForm	EMail, EMail2	
derive gUpd		EMail, EMail2	
derive gParse	EMail, EMail2	
derive gPrint	EMail, EMail2	

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
				, message`	:: !HtmlTextarea
				}

internalEmail2 :: (Task Void)
internalEmail2
=							[Text "Type your email message ..."] 
							?>>	editTask "Send" createDefault
	=>> \msg ->				msg.to` @: (msg.EMail2.subject`, [toHtml msg] ?>> ok)
	#>>						[Text "Mail has been read." ] ?>> ok



// BUGs found: filter for tabs does not seem to work ok

internalEmailResponse :: (Task Void)
internalEmailResponse = Cancel internalEmailResponse`
where
	internalEmailResponse`
	=							getCurrentUser
		>>= \(me,myname) ->		getToNames
		>>= \tos ->				[Text "Type your message ..."] 
								?>>	editTask "Commit" (initMsg (foldl (\s1 s2 -> s1 +++ "; " +++ s2) "" (map snd tos)) myname "" "")
		>>= \msg ->				myAndTasks [Text "Mail send to:"] 
										[ ("For: " <+++ toname <+++ "; Subject: " <+++ msg.subject
										, MailAndReply msg (me,myname) (to,toname))
										\\ (to,toname) <- tos
										]  
	where
		MailAndReply msg (me,myname) (to,toname)
		=						to @: 	( msg.subject
										, (showMSg msg ++ [Text "Reply requested: ", BrTag []])
										  ?>> editTask "Reply" (HtmlTextarea 4 "")
										)
			>>= \(HtmlTextarea _ reply)
						->		me @: ( "Reply from: " <+++ toname <+++ "; Subject: " <+++ msg.subject
									  , showMSg (initMsg myname toname ("RE: " <+++ msg.subject) reply) ?>> ok 
									  )

	
internalEmail :: (Task Void)
internalEmail
=							getCurrentUser
	>>= \(me,myname) ->		getToName
	>>= \(to,toname) ->		[Text "Type your message ..."] 
							?>>	editTask "Commit" (initMsg toname myname "" "")
	>>= \msg ->				showMSg msg ?>> (to @: (msg.subject, showMSg msg ?>> ok))

initMsg to for subject msg 
= {to = DisplayMode to, mailFrom = DisplayMode for, subject = subject , message = HtmlTextarea 4 msg}

showMSg msg
# (HtmlTextarea _ text) = msg.message
# (DisplayMode to)		= msg.to
# (DisplayMode for)		= msg.mailFrom

=	[ Text ("For : " <+++ to), 			BrTag [] 
	, Text ("From : " <+++ for), 		BrTag [] 
	, Text ("Subject : " <+++ msg.subject), BrTag [], HrTag [], BrTag []
	, Text text
	, BrTag [], BrTag [], HrTag [] 
	] 

addNewsGroup :: (Task Void)
addNewsGroup	= Cancel addNewsGroup` 
where
	addNewsGroup`
	=						readNewsGroups
		>>= \groups ->		(showCurrentNames groups ++ [Text "Enter new news group name to add:",BrTag []])
							?>> editTask "Define" ""
		>>= \newName ->		readNewsGroups
		>>= \groups ->		writeNewsGroups (removeDup (sort [newName:groups])) 
		>>= \groups ->		chooseTask (showCurrentNames groups ++ [Text "Do you want to add more ?"])
								[("Yes", addNewsGroup`)
								,("No",  return_V Void)
								]

showNewsGroups :: (Task Void)
showNewsGroups
=						readNewsGroups
	>>=	\groups	->		showCurrentNames groups ?>> ok 

showCurrentNames []		= [ Text "No names in catalogue yet !", BrTag [],BrTag []] 
showCurrentNames names	= [ Text "Current names in catalogue:", BrTag [],BrTag []
						  , toHtml (HtmlTextarea (min (length names) 5) (foldr (\s1 s2 -> s1 +++ "\n" +++ s2) "" names)), BrTag [], BrTag []
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
	=						[Text "Choose a group:", BrTag [],BrTag []] ?>> selectWithPulldown groups 0  
		>>= \index	->		return_V (groups!!index)
		>>= \group ->		addSubscription me (group,0)
		#>>					spawnProcess me True (group <+++ " news group subscription", readNews me group 0)
		#>>					return_V Void



readNews :: Int String Int -> Task Void
readNews me group index	
=			orTasks2 [Text ("Welcome to newsgroup " +++ group)]
							 [("Read next news items from newsgroup " <+++ group, readMore)
							 ,("Commit new message to newsgroup " <+++ group,	  commitItem group #>> return_V index)
							 ,("Unsubscribe from newsgroup " <+++ group,		  unsubscribe) 
							 ,("Message list of newsgroup " <+++ group, 		  messageList index #>> return_V index)  
							 ]
		>>= \index -> if (index >= 0)
						(spawnProcess me True (group <+++ " news group subscription", readNews me group index) #>> return_V Void) // CODE GENERATION BUG WHEN REPLACE BY #>> 
						(return_V Void)
where
	unsubscribe
	=						chooseTask [Text "Do you realy want to unsubscribe ?", BrTag [], BrTag []]
								[ ("Yes",return_V -1)
								, ("No",return_V index)
								]

	readMore 
	=						chooseTask [Text "Browse through messagelist...", BrTag [], BrTag []]
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
			= addSubscription me (group,nix) #>> return_V nix				 

	messageList index
	= 						readNewsGroup group 
		>>= \newsItems  ->	andTasks [("Message " <+++ i, show i newsItem) \\ newsItem <- newsItems%(index,index+nmessage-1) & i <- [index..]]
		#>>					editTask "Refresh list" Void

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
		=							[Text "Type your message ..."] 
									?>>	editTask "Commit" (HtmlTextarea 4  "") 
		 >>= \(HtmlTextarea _ val) -> 	readNewsGroup  group 
		 >>= \news ->				writeNewsGroup group (news ++ [(me,name,val)]) 
		 #>>							[Text "Message commited to news group ",BTag [] [Text group], BrTag [],BrTag []] 
									?>> ok

			
getToNames = getToNames` []
where
	getToNames` names 	
	=						showCurrentNames (map snd names)
							?>> getToName
		>>= \(id,name) ->	let newnames = [(id,name):names] in
							chooseTask (showCurrentNames (map snd newnames) ++ [BrTag [], Text "More names to add? ", BrTag []])
								[ ("Yes", getToNames` newnames)
								, ("No",  return_V    newnames)
								]

getToName ::  (Task (Int,String))
getToName 
= 						getUsers
	>>= \users ->		chooseTask_pdm [Text "Select user to mail a message to: "] 0
							[(name, return_V (userId,name)) \\ (userId,name) <- users]


Cancel :: (Task a) -> Task a | iData a
Cancel task
= 	orTasksV [("B",task),("O", cancelTask)]
where
	cancelTask = [HrTag []] ?>> editTask "Cancel Task" Void  #>> return_V createDefault


orTasks2 :: [HtmlTag] [LabeledTask a] -> Task a | iData a
orTasks2 msg taskCollection	
=	newTask "orTasks" (allTasksCond "orTask" (TTSplit msg) (\list -> length list >= 1) taskCollection)
	>>= \lista -> return_V (hd lista)
	
myAndTasks msg taskCollection	
=	newTask "orTasks" (allTasksCond "andTask" (TTSplit msg) (\_ -> False) taskCollection)
	>>= \lista -> return_V (hd lista)

// reading and writing of storages

newsGroupsId ::  (DBid NewsGroupNames)
newsGroupsId		=	mkDBid "newsGroups" LSTxtFile

readerId :: Int -> (DBid Subscriptions)
readerId i			= 	mkDBid ("reader" <+++ i) LSTxtFile

groupNameId :: String -> (DBid NewsGroup)
groupNameId name	=	mkDBid ("NewsGroup-" +++ name) LSTxtFile

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
	>>= \subscriptions ->	return_V (hds [index \\ (group,index) <- subscriptions | group == groupname])
where
	hds [x:xs] = x
	hds [] = 0

readNewsGroup :: GroupName -> Task NewsGroup
readNewsGroup groupname = readDB (groupNameId groupname)

writeNewsGroup :: GroupName NewsGroup -> Task NewsGroup
writeNewsGroup groupname news = writeDB (groupNameId groupname) news


