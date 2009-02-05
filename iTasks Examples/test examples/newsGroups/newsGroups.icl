module newsGroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create new newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news
// (c) mjp 2007 

import StdEnv, iTasks, iDataTrivial, iDataFormlib, iDataWidgets

derive gForm 	[]
derive gUpd 	[]

:: NewsGroups	:== [GroupName]					// list of newsgroup names
:: GroupName	:== String						// Name of the newsgroup
:: NewsGroup	:== [News]						// News stored in a news group
:: News			:== (Subscriber,Name,Message)	// id, name, and message of the publisher
:: Subscriber	:== Int							// the id of the publisher
:: Name			:== String						// the login name of the publisher
:: Message		:== String						// the message
:: Subscriptions:== [Subscription]				// newsgroup subscriptions of user
:: Subscription	:== (GroupName,Index)			// last message read in corresponding group
:: Index		:== Int							// 0 <= index < length newsgroup 

nmessage = 5

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

addNewsGroup
=						[Text "Define name of new news group:",BrTag [],BrTag []] 
						?>> editTask "Define" "" 
	=>> \newName  	->	readNewsGroups       
	=>> \oldNames 	->	writeNewsGroups (removeDup (sort [newName:oldNames])) 
	#>>					return_V Void

showNewsGroups
=						readNewsGroups
	=>>					showList
where
	showList []	 	=	[Text "No newsgroups in catalogue yet:", BrTag [],BrTag []] ?>> OK 
	showList list	=   selectWithPulldown list	0 
						#>> return_V Void

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
		#>>					spawnProcess me True (group <+++ " news group subscription",readNews me group)
		#>>					return_V Void



readNews :: Int String -> Task Void
readNews me group	
=						readIndex me  group 
		=>> \index ->	readNewsGroup group 
		=>> \news  ->	orTasks2 group
							([("Refresh",editTask "Refresh" Void)
							 ,("Commit new message to newsgroup " <+++ group,commitItem group)] ++
							  [("Message " <+++ i, show i who name message) \\ (who,name,message) <- news & i <- [index..]]
							 )
		#>> spawnProcess me True (group <+++ " news group subscription",readNews me group)
		#>> return_V Void
where
	show i who name message 
	= 	displayHtml 
			[ Text ("Message : " <+++ i), BrTag []
			, Text ("From    : " <+++ name) , BrTag [], HrTag []
			, Text message ]
		

	commitItem :: String ->(Task Void)
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



readNextNewsItems :: Subscriber Subscription Int Int -> Task Void
readNextNewsItems  me (group,index) offset length
# nix = index + offset
# nix = if (nix < 0) 0 (if (length <= nix) index nix)
= addSubscription me (group,nix) #>> return_V Void				 
		


OK :: Task Void
OK = editTask "OK" Void



orTasks2 group taskCollection	
	= newTask "orTasks" (allTasksCond "orTask" (TTSplit msg) (\list -> length list >= 1) taskCollection)
							=>> \list -> return_V (hd list)
where
	msg = [Text ("Welcome to newsgroup " +++ group)]


// reading and writing of storages

newsGroupsId ::  (DBid NewsGroups)
newsGroupsId		=	mkDBid "newsGroups" LSTxtFile

readerId :: Int -> (DBid Subscriptions)
readerId i			= 	mkDBid ("reader" <+++ i) LSTxtFile

groupNameId :: String -> (DBid NewsGroup)
groupNameId name	=	mkDBid ("NewsGroup-" +++ name) LSTxtFile

readNewsGroups :: Task NewsGroups
readNewsGroups = readDB newsGroupsId

writeNewsGroups :: NewsGroups -> Task NewsGroups
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


