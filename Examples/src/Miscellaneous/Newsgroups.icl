implementation module Newsgroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create new newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news
// (c) mjp 2007 

import StdList, StdOrdList, StdTuple, StdMisc
import iTasks
import CommonDomain

:: NewsGroupNames:== [GroupName]				// list of newsgroup names
:: GroupName	:== String						// Name of the newsgroup
:: NewsGroup	:== [NewsItem]					// News stored in a news group
:: NewsItem		:== (Subscriber,Message)		// id, name, and message of the publisher
:: Subscriber	:== UserName					// the id of the publisher
:: Name			:== String						// the login name of the publisher
:: Message		:== String						// the message
:: Subscriptions:== [Subscription]				// newsgroup subscriptions of user
:: Subscription	:== (GroupName,Index)			// last message read in corresponding group
:: Index		:== Int							// 0 <= index < length newsgroup 


derive gPrint		EMail, Reply
derive gParse		EMail, Reply
derive gVisualize	EMail, Reply	
derive gUpdate		EMail, Reply

derive bimap		Maybe, (,)

nmessage = 2

newsgroupsExample :: [Workflow]
newsgroupsExample
=	[	/*workflow	 "Examples/Miscellaneous/Newsgroups" newsGroup
	,	*/workflow	 "Examples/Mail/Internal/With receive confirmation" internalEmail
	,	workflow	 "Examples/Mail/Internal/With forced reply" internalEmailReply
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
	>>= \msg ->						msg.to @: (msg.EMail.subject, showMessageAbout "You have received the following message:" msg)
	>>|								showMessage ("Your mail has been read by " <+++ getUserName msg.to)
	>>|								return msg

getUserName (UserName id name) = name
	
internalEmailReply :: (Task (EMail,Reply)) // crashes ?? group
internalEmailReply
=									enterInformation "Type your email message ..."
	>>= \msg ->						msg.to @: (msg.EMail.subject, (showMessageAbout "You have received the following message:" msg
																  ||- 
																   enterInformation "The sender requested a reply..."))
	>>= \reply->					showMessageAbout ("Your mail has been read by " <+++ getUserName msg.to) reply
	>>|								return (msg,reply)


myAndTasks msg tasks =	oldParallel "andTask" (\_ -> False) undef hd [t <<@ l \\(l,t) <- tasks]

showCurrentNames :: [UserName] -> Task Void
showCurrentNames names = showStickyMessageAbout "Current names:" names

cancel :: (Task a) -> Task a | iTask a
cancel task = task -||- (showMessage "Cancel this task" >>| getDefaultValue)

// newsgroup handling

/*


ifValid expr = Predicate (\val -> case val of
									Invalid -> False
									_ -> expr)
initMenu :: Task Void
initMenu 
	= setMenus
		[ Menu "File"	[ MenuItem "New"			ActionNew
						, MenuItem "Open..."		ActionOpen
						, MenuSeparator
						, MenuItem "Save"			ActionSave
						, MenuItem "Save As..."		ActionSaveAs
						, MenuSeparator
						, MenuItem "Quit"			ActionQuit
						]
		, Menu "Help"	[ MenuItem "About"			ActionShowAbout 
						]
		]

actions ((name,flow), mode)
	=	map MenuAction	[ (ActionNew,		Always)
						, (ActionOpen,		Always)
						, (ActionSave,		ifValid (validFlow name flow.flowDyn))
						, (ActionSaveAs,	ifValid (validFlow name flow.flowDyn))
						, (ActionQuit,		Always)
						, (ActionShowAbout,	Always)
						]

validFlow name flowDyn = name <> "" && (validTaskFun flowDyn || validTask flowDyn)


handleMenu :: Task Void
handleMenu 
	=	initMenu >>| doMenu emptyState

doMenu state=:((name,flow), mode)
		=	case mode of
				False 		->							updateInformationA title1 (actions state) Void 
								>>= \(action,_) ->		return (action,state)
				True 	->								updateInformationA title2 	[ ButtonAction (ActionSave, ifValid (validFlow name flow.flowDyn))
																					, ButtonAction (ActionOk, IfValid)
																					: actions state
																					] flow.flowShape
								>>= \(action,shape) ->  return (action,((name,{flow & flowShape = shape}),mode))
			>>= switchAction
where
	title1 = "No flow..."
	title2 = "Flow: \"" +++ name +++ "\" " +++ 
				if (validTaskFun flow.flowDyn || validTask flow.flowDyn) 
					(" :: " +++ showDynType flow.flowDyn) 
					(" :: " +++ typeErrorMess "Invalid Type, " flow.flowDyn)

switchAction (action, (nameflow=:(name,flow),mode))
	=	case action of
			ActionNew		-> 						newFlowName emptyFlow 	
								>>= \nameflow -> 	doMenu (nameflow,True)	
			ActionOpen		->						chooseFlow 	
								>>= \(name,flow) -> if (name == "")
														(doMenu (nameflow,False))
														(doMenu ((name,flow),True))
			ActionSave		->						storeFlow nameflow 	
								>>= \nameflow -> 	doMenu (nameflow,mode)
			ActionSaveAs	->						newFlowName flow 
								>>= \nameflow -> 	doMenu (nameflow,mode)
			ActionQuit		->						return Void
			ActionShowAbout	->						showAbout 
								>>| 				doMenu (nameflow,mode)
			ActionOk		->						try (flowShapeToFlow flow.flowShape) 
														(errorRaised flow.flowShape) 
								>>= \flow -> 		doMenu ((name,flow), mode)




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
	>>= 			subscribe (toUserName user)
where
	subscribe me []
	=						showMessage "No newsgroups in catalogue yet:"
	subscribe me groups
	=						enterChoice "Choose a group:" groups
		>>= \group ->		addSubscription me (group,0)
		>>|					spawnProcess me True (readNews me group 0 <<@ group <+++ " news group subscription")
		>>|					return Void



readNews :: UserName String Int -> Task Void
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
	show i (who, message) 
	= 	showMessageAbout [Text ("Message: " <+++ i), BrTag [], Text ("From: " <+++ toString who)] message
		

	commitItem :: String -> Task Void
	commitItem  group
	=								getCurrentUser
		>>= \user ->      			commit user group
	where
		commit me group
		=							enterInformation [Text "Type your message ..."] 
		 >>= \(Note val) -> 		readNewsGroup  group 
		 >>= \news ->				writeNewsGroup group (news ++ [(toUserName me,val)]) 
		 >>|						showMessage [Text "Message commited to news group ",BTag [] [Text group], BrTag [],BrTag []] 

			



orTasks2 :: [HtmlTag] [LabeledTask a] -> Task a | iTask a
orTasks2 msg tasks = oldParallel "orTasks2"  (\list -> length list >= 1) hd undef [t <<@ l \\(l,t) <- tasks] 


// reading and writing of storages

newsGroupsId ::  (DBid NewsGroupNames)
newsGroupsId		=	mkDBid "newsGroups"

readerId :: UserName -> (DBid Subscriptions)
readerId name		= 	mkDBid ("Reader-" <+++ name)

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

*/
/*
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
										, MailAndReply msg (me.User.userName,me.User.displayName) (to,toname))
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


getToNames = getToNames` []
where
	getToNames` names 	
	=						showCurrentNames names
								||- getToName
		>>= \name	 ->		let newnames = [name:names] in
								showCurrentNames newnames
								||- requestConfirmation "Add more names?"
								>>= \yn ->
									if yn (getToNames` newnames) (return newnames)
getToName ::  (Task UserName)
getToName 
= 						getUsers
	>>= \users ->		enterChoice "Select user to mail a message to: " users
	>>= \user ->		return (toUserName user)

*/
