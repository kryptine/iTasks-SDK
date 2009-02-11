module newsGroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create new newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news
// (c) mjp 2007 

import StdEnv, iTasks, iDataTrivial, iDataFormlib, iDataWidgets


:: NewsGroupNames:== [GroupName]					// list of newsgroup names
:: GroupName	:== String						// Name of the newsgroup
:: NewsGroup	:== [NewsItem]						// News stored in a news group
:: NewsItem		:== (Subscriber,Name,Message)	// id, name, and message of the publisher
:: Subscriber	:== Int							// the id of the publisher
:: Name			:== String						// the login name of the publisher
:: Message		:== String						// the message
:: Subscriptions:== [Subscription]				// newsgroup subscriptions of user
:: Subscription	:== (GroupName,Index)			// last message read in corresponding group
:: Index		:== Int							// 0 <= index < length newsgroup 

nmessage = 2

Start world = startEngine newsGroups world

newsGroups
=	[
	{	name		= "add news group"
	,	label		= "add news group"
	,	roles		= []
	,	mainTask	= addNewsGroup
	},
	{	name		= "show news groups"
	,	label		= "show news groups"
	,	roles		= []
	,	mainTask	= showNewsGroups
	},
	{	name		= "subscribe to news group"
	,	label		= "subscribe to news group"
	,	roles		= []
	,	mainTask	= subscribeNewsGroup
	}
	]

addNewsGroup :: (Task Void)
addNewsGroup	= Cancel addNewsGroup` 
where
	addNewsGroup`
	=						readNewsGroups
		=>> \groups ->		(showCurrentGroups groups ++ [Text "Enter new news group name to add:",BrTag []])
							?>> editTask "Define" ""
		=>> \newName ->		readNewsGroups
		=>> \groups ->		writeNewsGroups (removeDup (sort [newName:groups])) 
		=>> \groups ->		chooseTask (showCurrentGroups groups ++ [Text "Do you want to add more ?"])
								[("Yes", addNewsGroup`)
								,("No",  return_V Void)
								]

showNewsGroups :: (Task Void)
showNewsGroups
=						readNewsGroups
	=>>	\groups	->		showCurrentGroups groups ?>> OK 

showCurrentGroups:: NewsGroupNames -> [HtmlTag]
showCurrentGroups []		= [ Text "No newsgroups in catalogue yet !", BrTag [],BrTag []] 
showCurrentGroups groups	= [ Text "Current newsgroups in catalogue:", BrTag [],BrTag []
						  	  , toHtml (HtmlTextarea (min (length groups) 5) (foldl (\s1 s2 -> s1 +++ "\n" +++ s2) "" groups)), BrTag [], BrTag []
						  	  ]

subscribeNewsGroup :: (Task Void)
subscribeNewsGroup
=						getCurrentUserId
	=>> \me ->			getDisplayNamesTask [me]
	=>> \names ->      	readNewsGroups 
	=>> 				subscribe me (hd names)
where
	subscribe me myname []
	=						[Text "No newsgroups in catalogue yet:", BrTag [],BrTag []] ?>> OK 
	subscribe me myname groups
	=						[Text "Choose a group:", BrTag [],BrTag []] ?>> selectWithPulldown groups 0  
		=>> \index	->		return_V (groups!!index)
		=>> \group ->		addSubscription me (group,0)
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
		=>> \index -> if (index >= 0)
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
			=>> \index ->	readNewsGroup group
			=>> \news ->	readNextNewsItems index offset (length news)
		where
			readNextNewsItems  index offset length
			# nix = index + offset
			# nix = if (nix < 0) 0 (if (length <= nix) index nix)
			= addSubscription me (group,nix) #>> return_V nix				 

	messageList index
	= 						readNewsGroup group 
		=>> \newsItems  ->	andTasks [("Message " <+++ i, show i newsItem) \\ newsItem <- newsItems%(index,index+nmessage-1) & i <- [index..]]
		#>>					editTask "Refresh list" Void

	show :: Int NewsItem -> Task Void
	show i (who, name, message) 
	= 	[ Text ("Message : " <+++ i), BrTag []
		, Text ("From    : " <+++ name) , BrTag [], HrTag []
		, Text message ] ?>> OK
		

	commitItem :: String -> Task Void
	commitItem  group
	=								getCurrentUserId
		=>> \me ->					getDisplayNamesTask [me]
		=>> \names ->      			commit me (hd names) group
	where
		commit me name group
		=							[Text "Type your message ..."] 
									?>>	editTask "Commit" (HtmlTextarea 4  "") 
		 =>> \(HtmlTextarea _ val) -> 	readNewsGroup  group 
		 =>> \news ->				writeNewsGroup group (news ++ [(me,name,val)]) 
		 #>>							[Text "Message commited to news group ",BTag [] [Text group], BrTag [],BrTag []] 
									?>> OK

		


OK :: Task Void
OK = editTask "OK" Void

Cancel :: (Task a) -> Task a | iData a
Cancel task
= 	orTasksV [("B",cancelTask),("O", task)]
where
	cancelTask = editTask "Cancel Task" Void <<? [HrTag []] #>> return_V createDefault


orTasks2 :: [HtmlTag] [LabeledTask a] -> Task a | iData a
orTasks2 msg taskCollection	
=	newTask "orTasks" (allTasksCond "orTask" (TTSplit msg) (\list -> length list >= 1) taskCollection)
	=>> \lista -> return_V (hd lista)
	

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
	=>> \subscriptions -> 	writeSubscriptions me [(groupname,index):[(group,index) \\ (group,index) <- subscriptions | group <> groupname]]

readIndex :: Subscriber GroupName -> Task Index
readIndex me groupname
= 							readSubscriptions me 
	=>> \subscriptions ->	return_V (hds [index \\ (group,index) <- subscriptions | group == groupname])
where
	hds [x:xs] = x
	hds [] = 0

readNewsGroup :: GroupName -> Task NewsGroup
readNewsGroup groupname = readDB (groupNameId groupname)

writeNewsGroup :: GroupName NewsGroup -> Task NewsGroup
writeNewsGroup groupname news = writeDB (groupNameId groupname) news


