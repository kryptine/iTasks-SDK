implementation module Messaging

import iTasks
import CommonDomain
import HRM
import GenEq

messaging :: [Workflow]
messaging 
	= [ workflow "Messaging/Send a new Message" newMessage 
	  , workflow "Messaging/Send a new Group-Message" newMessageToGroup
	  , workflow "Messaging/View Message Archive" viewArchive
	  ]

//========================================================================================================================================================================
// Internal mail
//========================================================================================================================================================================

derive class iTask Message
derive gEq Message, HtmlDisplay, Maybe, User, Document, Note, TaskPriority, UserDetails, Password

derive bimap Maybe, (,)

:: MsgDB :== [Message]

:: Message =
	{ sender			:: HtmlDisplay User
	, to 				:: [User]
	, cc 				:: Maybe [User]
	, priority 			:: TaskPriority
	, subject			:: String
	, message			:: Note
	, attachments		:: Maybe [Document]
	, previousMessages 	:: HtmlDisplay [Message]
	}

instance == Message
where (==) msgA msgB = msgA === msgB

mkMsg :: User -> Message
mkMsg me = { Message
			| sender 		= (toHtmlDisplay me)
			, to 			= []
			, cc 			= Nothing
			, priority 		= NormalPriority
			, subject		= ""
			, message		= Note ""
		  	, attachments	= Nothing
		   	, previousMessages = HtmlDisplay []
		   	}

msgDBid :: (DBid MsgDB)
msgDBid = mkDBid "msgDB"

newMessage :: Task Void
newMessage = getCurrentUser
	>>= \me -> 		writeMessage me "" [] [] []
	>>= \msg -> 	sendMessage msg

newMessageToGroup :: Task Void
newMessageToGroup = getCurrentUser
	>>= \me ->		getUserGroups
	>>= \groups ->	enterChoice "Select group" groups
	>>= \role ->	getUsersWithRole role
	>>= \users ->	writeMessage me "" users [] []
	>>= \msg ->		sendMessage msg
	
sendMessage :: Message -> Task Void
sendMessage msg = allProc [who @>> spawnProcess who True True
					((readMessage msg <<@ Subject ("Message from "+++toString (fromHtmlDisplay msg.Message.sender)+++": "+++msg.Message.subject)) <<@ msg.Message.priority) \\ who <- (msg.Message.to ++ if(isJust msg.cc) (fromJust msg.cc) [])] Closed
					>>| showMessageAbout "The following message has been sent:" msg

writeMessage :: User String [User] [User] [Message] -> Task Message
writeMessage me subj to cc thread = updateInformation "Enter your message" {Message | (mkMsg me) & subject = subj, to = to, cc = if(isEmpty cc) Nothing (Just cc), previousMessages = (HtmlDisplay thread)}	

readMessage :: Message -> Task Void
readMessage msg=:{Message | previousMessages} 
	= showMessageAboutA "You received a message" [ButtonAction (ActionLabel "Reply",Always), 
		ButtonAction (ActionLabel "Reply All",Always), ButtonAction (ActionLabel "Forward",Always), ButtonAction (ActionLabel "Delete", Always), ButtonAction (ActionLabel "Archive & Close",Always)] msg
	>>= \act -> case act of
		(ActionLabel "Reply")
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: "+++msg.Message.subject) [(fromHtmlDisplay msg.sender)] []  [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
			>>= \msg -> sendMessage msg
		(ActionLabel "Reply All")
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: "+++msg.Message.subject) [(fromHtmlDisplay msg.sender):[u \\ u <- msg.to | u <> me]] (if(isJust msg.cc) (fromJust msg.cc) []) [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
			>>= \msg -> sendMessage msg
		(ActionLabel "Forward")
			= 			getCurrentUser 
			>>= \me -> 	writeMessage me ("Fw: "+++msg.Message.subject) [] [] [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
			>>= \msg -> sendMessage msg
		(ActionLabel "Archive & Close") 
			= 			readDB msgDBid
			>>= \mdb -> writeDB msgDBid (removeDup [msg:mdb])
			>>| 		showMessage "Message stored in archive"
		(ActionLabel "Delete")
			=			readDB msgDBid
			>>= \mdb -> writeDB msgDBid (filter (\dbmsg -> dbmsg <> msg) mdb)
			>>|			showMessage "Message deleted"

viewArchive :: Task Void
viewArchive = getCurrentUser
	>>= \me ->	readDB msgDBid
	>>= \mdb -> selectMsg mdb me
	>>= \sel -> allProc [spawnProcess me True True ((readMessage msg <<@ Subject ("Message from "+++toString (fromHtmlDisplay msg.Message.sender)+++": "+++msg.Message.subject)) <<@ msg.Message.priority) \\ msg <- sel] Closed
	>>| return Void
where
	selectMsg :: MsgDB User -> Task [Message]
	selectMsg mdb me
		# mdbs = filter (\msg -> (isMember me msg.to) || (isMember me (if(isJust msg.cc) (fromJust msg.cc) []))) mdb
		= case mdb of
			[] = showMessage "The archive is empty" >>| return []
			_  = enterMultipleChoice "Which messages do you want to view?" mdbs

//========================================================================================================================================================================
// Broadcasting
//========================================================================================================================================================================

broadcast :: [User] String (Maybe a) -> Task Void | iTask a
broadcast to msg mbAbout = allProc [spawnProcess who True True show \\ who <- to] Closed >>| return Void
where
	show = case mbAbout of
		Just a = showMessageAbout msg a
		Nothing = showMessage msg