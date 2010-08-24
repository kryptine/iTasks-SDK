implementation module Messaging

import iTasks
import CommonDomain
import Groups
import GenEq


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

msgDBid :: (DBId MsgDB)
msgDBid = mkDBId "msgDB"

newMessage :: Task Void
newMessage = getCurrentUser
	>>= \me -> 		writeMessage me "" [] [] []
	>>= \msg -> 	sendMessage msg

newMessageToGroup :: Task Void
newMessageToGroup = getCurrentUser
	>>= \me ->		getMyGroups
	>>= \groups ->	case groups of
		[]	=	showMessage "No groups" "You are not a member of any group" Void
		_	=	enterChoice "Choose group" "Select group" groups
			>>= \group ->	writeMessage me "" group.members [] []
			>>= \msg ->		sendMessage msg
	
sendMessage :: Message -> Task Void
sendMessage msg = allProc [who @>> spawnProcess who True True
					((readMessage msg <<@ Subject ("Message from "+++toString (fromHtmlDisplay msg.Message.sender)+++": "+++msg.Message.subject)) <<@ msg.Message.priority) \\ who <- (msg.Message.to ++ if(isJust msg.cc) (fromJust msg.cc) [])] Closed
					>>| showMessageAbout "Message sent" "The following message has been sent:" msg >>| return Void

writeMessage :: User String [User] [User] [Message] -> Task Message
writeMessage me subj to cc thread = updateInformation "Compose" "Enter your message" {Message | (mkMsg me) & subject = subj, to = to, cc = if(isEmpty cc) Nothing (Just cc), previousMessages = (HtmlDisplay thread)}	

readMessage :: Message -> Task Void
readMessage msg=:{Message | previousMessages, subject} 
	= showMessageAboutA subject "You received a message" [ButtonAction (ActionLabel "Reply",Always), 
		ButtonAction (ActionLabel "Reply All",Always), ButtonAction (ActionLabel "Forward",Always), ButtonAction (ActionLabel "Delete", Always), ButtonAction (ActionLabel "Archive & Close",Always)] msg
	>>= \act -> case act of
		(ActionLabel "Reply",_)
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: "+++msg.Message.subject) [(fromHtmlDisplay msg.sender)] []  [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
			>>= \msg -> sendMessage msg
		(ActionLabel "Reply All",_)
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: "+++msg.Message.subject) [(fromHtmlDisplay msg.sender):[u \\ u <- msg.to | u <> me]] (if(isJust msg.cc) (fromJust msg.cc) []) [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
			>>= \msg -> sendMessage msg
		(ActionLabel "Forward",_)
			= 			getCurrentUser 
			>>= \me -> 	writeMessage me ("Fw: "+++msg.Message.subject) [] [] [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
			>>= \msg -> sendMessage msg
		(ActionLabel "Archive & Close",_) 
			= 			readDB msgDBid
			>>= \mdb -> writeDB msgDBid (removeDup [msg:mdb])
			>>| 		showMessage "Archived" "Message stored in archive" Void
		(ActionLabel "Delete",_)
			=			readDB msgDBid
			>>= \mdb -> writeDB msgDBid (filter (\dbmsg -> dbmsg <> msg) mdb)
			>>|			showMessage "Deleted" "Message deleted" Void

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
			[] = showMessage "Empty archive" "The archive is empty" []
			_  = enterMultipleChoice "Select messages" "Which messages do you want to view?" mdbs

//========================================================================================================================================================================
// Broadcasting
//========================================================================================================================================================================

broadcast :: [User] String (Maybe a) -> Task Void | iTask a
broadcast to msg mbAbout = allProc [spawnProcess who True True show \\ who <- to] Closed >>| return Void
where
	show = case mbAbout of
		Just a = showMessageAbout "TODO" msg a >>| return Void
		Nothing = showMessage "TODOD" msg Void