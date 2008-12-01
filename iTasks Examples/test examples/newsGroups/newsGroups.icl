module newsGroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create new newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news
// (c) mjp 2007 

import StdEnv, StdiTasks, iDataTrivial, iDataFormlib, iDataWidgets

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

Start world = startTaskEngine (doWork 0 "Rinus") world

doWork 0 acc =  newsManager 0 acc					// for the root
doWork i acc =  newsReader  i acc		// all others

newsManager i name
=							spawnWorkflow i True ("subscribe",newsReader i name)
	#>>						manageGroups
where
	manageGroups
	=						foreverTask
							(	chooseTask [Text "news group management:",BrTag [],BrTag []] 
									[ ("add new group",  addNewsGroup -||- editTask "Cancel" Void)
								  	, ("show groups", showGroups)
								 	]
							)
	addNewsGroup
	=						[Text "Define name of new news group:",BrTag [],BrTag []] 
							?>> editTask "Define" "" 
		=>> \newName  	->	readNewsGroups       
		=>> \oldNames 	->	writeNewsGroups (removeDup (sort [newName:oldNames])) 
		#>>					return_V Void

	showGroups
	=						readNewsGroups
		=>>					showList
	where
		showList []	 	=	[Text "No newsgroups in catalogue yet:", BrTag [],BrTag []] ?>> OK 
		showList list	=   PDMenu list	#>> return_V Void

newsReader unid name
=	foreverTask		( chooseTask [Text "subscribe to a news group from the cataloque:",BrTag [],BrTag []] 
						[("show groups", subscribeNewsGroup unid name -||- editTask "Cancel" Void)]
				  	)
where
	subscribeNewsGroup :: Subscriber String -> Task Void
	subscribeNewsGroup me name
	=						readNewsGroups 
		=>> 				subscribe
	where
		subscribe []
		=						[Text "No newsgroups in catalogue yet:", BrTag [],BrTag []] ?>> OK 
		subscribe groups
		=						[Text "Choose a group:", BrTag [],BrTag []] ?>> PDMenu groups  
			=>> \(_,group)	->	addSubscription me (group,0)
			#>>					spawnWorkflow me True (group,readNews me group)
			#>> 				[Text "You have subscribed to news group ", BTag [] [Text group],BrTag [],BrTag []] 
								?>> OK

	readNews me group	=	[Text "You are looking at news group ", BTag [] [Text group], BrTag [], BrTag []] 
							?>>	foreverTask 
							(					readIndex me  group 
								=>> \index ->	readNewsGroup group 
								=>> \news  ->	showNews index (news%(index,index+nmessage-1)) (length news) 
												?>>	 chooseTask []	
														[("<<",			readNextNewsItems me (group,index) (~nmessage) (length news))
														,("update",		return_V Void)
														,(">>",			readNextNewsItems me (group,index) nmessage (length news))
														,("commitNews",	commitItem group me)
														,("unsubscribe",deleteMe)
														]
							)

	readNextNewsItems :: Subscriber Subscription Int Int -> Task Void
	readNextNewsItems  me (group,index) offset length
	# nix = index + offset
	# nix = if (nix < 0) 0 (if (length <= nix) index nix)
	= addSubscription me (group,nix) #>> return_V Void				 

	commitItem :: GroupName Subscriber -> Task Void
	commitItem group me 
	=								[Text "Type your message ..."] 
									?>>	editTask "Commit" (HtmlTextarea 4  "") <<@ Submit 
		=>>	\(HtmlTextarea _ val) -> 	readNewsGroup  group 
		=>> \news ->				writeNewsGroup group (news ++ [(unid,name,val)]) 
		#>>							[Text "Message commited to news group ",BTag [] [Text group], BrTag [],BrTag []] 
									?>> OK

OK :: Task Void
OK = editTask "OK" Void

PDMenu list
=							[] 
							?>> editTask "OK" (HtmlSelect [(e,e) \\ e <- list] (toString 0))
	=>> \(HtmlSelect _ value) 
						->	return_V (toInt value, value)

// displaying news groups

showNews ix news nrItems = [STable [BorderAttr (toString 1), BgcolorAttr "Blue"] 	
								[	[BTag [] [Text "Message nr:"], BTag [] [Text "By:"], BTag [] [Text "Contents:"]]
								:	[ 	[Text (showIndex nr),Text name,Text (toString info)] 
									  	 \\ nr <- [ix..] & (who,name,info) <- news
										]
									 ]  
								]
where
	showIndex i	= ((i+1) +++> " of ") <+++ nrItems
	


STable atts table		= TableTag atts (mktable table)
where
	mktable table 	= [TrTag [] (mkrow rows)           \\ rows <- table]
	mkrow   rows 	= [TdTag [ValignAttr "top"]  [row] \\ row  <- rows ]


// reading and writing of storages

newsGroupsId ::  (DBid NewsGroups)
newsGroupsId		=	mkDBid "newsGroups" TxtFile

readerId :: Int -> (DBid Subscriptions)
readerId i			= 	mkDBid ("reader" <+++ i) TxtFile

groupNameId :: String -> (DBid NewsGroup)
groupNameId name	=	mkDBid ("NewsGroup-" +++ name) TxtFile

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


