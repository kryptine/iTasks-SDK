implementation module Toolbox

/*
	This toolbox contains a number of workflows which can be handle unforseen situations. Or can be used as 'units' in other workflows.
*/

import iTasks
import CommonDomain

from HRM import getUserRoles

derive gPrint Message
derive gParse Message
derive gVisualize Message
derive gUpdate Message
derive gHint Message
derive gMakeLocalCopy Message, TaskPriority

derive gEq Message, HtmlDisplay, User, Note, TaskPriority, UserDetails, Password

derive bimap Maybe, (,)

gError{|Message|} msg=:{Message | to} est
	#est = case to of [] = labeledChild "to" (appendError "Please select at least one recipient" MPAlways) est; _ = est 
	= stepOut est

toolbox :: [Workflow]
toolbox 
	= [ workflow "Messaging/Send a new Message" newMessage 
	  , workflow "Messaging/Send a new Group-Message" newMessageToGroup
	  , workflow "Messaging/View Message Archive" viewArchive
	  , workflow "Toolbox/Pick a date" pickADate
	  ]

//========================================================================================================================================================================
// Internal mail
//========================================================================================================================================================================

:: MsgDB :== [Message]

:: Message =
	{ sender			:: HtmlDisplay User
	, to 				:: [User]
	, cc 				:: [User]
	, priority 			:: TaskPriority
	, subject			:: String
	, message			:: Note
	, attachments		:: [Document]
	, previousMessages 	:: HtmlDisplay [Message]
	}

instance == Message
where (==) msgA msgB = msgA === msgB

mkMsg :: User -> Message
mkMsg me = { Message
			| sender 		= (toHtmlDisplay me)
			, to 			= []
			, cc 			= []
			, priority 		= NormalPriority
			, subject		= ""
			, message		= Note ""
		  	, attachments	= []
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
	>>= \me ->		enterChoice "Select group" getUserRoles
	>>= \role ->	getUsersWithRole role
	>>= \users ->	writeMessage me "" users [] []
	>>= \msg ->		sendMessage msg
	
sendMessage :: Message -> Task Void
sendMessage msg = allProc [who @>> spawnProcess who True 
					((readMessage msg <<@ Subject ("Message from "+++toString (fromHtmlDisplay msg.Message.sender)+++": "+++msg.Message.subject)) <<@ msg.Message.priority) \\ who <- (msg.Message.to++msg.Message.cc)] Closed
					>>| showMessageAbout "The following message has been sent:" msg

writeMessage :: User String [User] [User] [Message] -> Task Message
writeMessage me subj to cc thread = updateInformation "Enter your message" {Message | (mkMsg me) & subject = subj, to = to, cc = cc, previousMessages = (HtmlDisplay thread)}	

readMessage :: Message -> Task Void
readMessage msg=:{Message | previousMessages} = showMessageAboutA "You received a message" [ButtonAction (ActionLabel "Reply",Always), 
		ButtonAction (ActionLabel "Reply All",Always), ButtonAction (ActionLabel "Forward",Always), ButtonAction (ActionLabel "Delete", Always), ButtonAction (ActionLabel "Archive & Close",Always)] msg
	>>= \act -> case act of
		(ActionLabel "Reply")
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: "+++msg.Message.subject) [(fromHtmlDisplay msg.sender)] []  [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
			>>= \msg -> sendMessage msg
		(ActionLabel "Reply All")
			= 			getCurrentUser
			>>= \me	->	writeMessage me ("Re: "+++msg.Message.subject) [(fromHtmlDisplay msg.sender):[u \\ u <- msg.to | u <> me]] msg.cc [{Message | msg & previousMessages = (HtmlDisplay [])}:fromHtmlDisplay previousMessages]
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
	>>= \sel -> allProc [spawnProcess me True ((readMessage msg <<@ Subject ("Message from "+++toString (fromHtmlDisplay msg.Message.sender)+++": "+++msg.Message.subject)) <<@ msg.Message.priority) \\ msg <- sel] Closed
	>>| return Void
where
	selectMsg :: MsgDB User -> Task [Message]
	selectMsg mdb me
		# mdbs = filter (\msg -> (isMember me msg.to) || (isMember me msg.cc)) mdb
		= case mdb of
			[] = showMessage "The archive is empty" >>| return []
			_  = enterMultipleChoice "Which messages do you want to view?" mdbs

//========================================================================================================================================================================
// Broadcasting
//========================================================================================================================================================================

broadcast :: [User] String (Maybe a) -> Task Void | iTask a
broadcast to msg mbAbout = allProc [spawnProcess who True show \\ who <- to] Closed >>| return Void
where
	show = case mbAbout of
		Just a = showMessageAbout msg a
		Nothing = showMessage msg

//========================================================================================================================================================================
// DatePicker
//========================================================================================================================================================================

:: DateVotes :== (HtmlDisplay [DateVote])

:: DateVote = 
	{ date :: Date
	, vote :: (Editable Vote)
	}

:: Vote = Yes | No | Maybe

:: VoteCount =
	{ date 	:: Date
	, yes 	:: Int
	, no 	:: Int
	, maybe :: Int
	}
	
derive gPrint		DateVote, Vote, VoteCount
derive gParse		DateVote, Vote, VoteCount
derive gUpdate		DateVote, Vote, VoteCount
derive gVisualize	DateVote, Vote, VoteCount
derive gError		DateVote, Vote, VoteCount
derive gHint		DateVote, Vote, VoteCount

derive gEq			Date

pickADate :: Task Void
pickADate = enterInformation "What is the subject?"
	>>= \subj ->	enterInformation "Who should be managing the descision?"
	>>= \ref ->		enterInformation "Who should be involved in the descision?"
	>>= \oth ->		(ref @: datePicker [ref:oth])
	>>= \date ->	broadcast [ref:oth] ("The chosen date for "+++subj+++": ") (Just date)

datePicker :: [User] -> Task Date
datePicker users =  pickDates
	>>= \dates -> allProc [u @>> voteDates dates \\ u <- users] Open
	>>= \votes -> pickFinal votes
where
	pickDates :: Task [Date]
	pickDates = enterInformation "Please select date options"

	voteDates :: [Date] -> Task DateVotes
	voteDates dates = updateInformation "Please indicate your preference" (HtmlDisplay [{DateVote | date = d, vote = (Editable Maybe)} \\ d <- dates])

	pickFinal :: [DateVotes] -> Task Date
	pickFinal votes
		# v = (map fromHtmlDisplay votes)
		# init = [{VoteCount | date = dv.DateVote.date, yes = 0, no = 0, maybe = 0} \\ dv <- (hd v)]
		# overview = foldl countVotes init v
		=   enterChoice "Please select the final option" overview
		>>= \final -> return final.VoteCount.date
	where
		countVotes votecount [] = votecount
		countVotes votecount [d:ds]
			# votecount = [if(d.DateVote.date === vc.VoteCount.date) (updateCount vc d.vote) vc \\ vc <- votecount]
			= countVotes votecount ds
			
		updateCount vc=:{yes,no,maybe} (Editable v)
			= case v of
				Yes 	= {vc & yes 	= inc yes}
				No  	= {vc & no  	= inc no}
				Maybe	= {vc & maybe 	= inc maybe}

//========================================================================================================================================================================
// ToDo-list
//========================================================================================================================================================================
