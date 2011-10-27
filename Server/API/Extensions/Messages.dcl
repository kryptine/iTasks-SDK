definition module Messages
/**
* This extension provides a simple e-mail-ish internal message system.
* If you receive messages they show up as tasks in your worklist.
* If you send messages you can choose to actively await a reply which will
* create a task in your worklist that is not completed until you receive a
* reply.
*/

import iTasks

:: Message =
	{ messageId			:: Hidden Int
	, subject			:: String
	, sender			:: Display User
	, recipients 		:: [User]
	, priority 			:: TaskPriority
	, needsReply		:: Bool
	, message			:: Note
	, attachments		:: Maybe [Document]
	, thread 			:: Display [Message]
	}

/**
* Top level workflow for managing your received messages and starting
* point for writing new messages.
*/
manageMessages		:: 											Task Void
/**
* Top level flow for viewing a single message
*
* @param The message
*
* @return True if a reply has been sent
*/
manageMessage 		:: !Message 								->	Task Bool
/**
* Combination of writing a new message followed by sending it.
*/
newMessage 		  	::											Task Void
/**
* Combination of choosing a group to send a message to,
* writing the message and sending it.
*/
newGroupMessage		::											Task Void
/**
* Composition of a new message.
*
* @param The sender of the message
* @param The initial subject of the message
* @param If this message is a followup of another message, that message
*
* @param The new message (not stored in the database yet)
*/
writeMessage		:: User String [User] (Maybe Message)	->	Task Message
/**
* Sending of a message.
* This stores the message to the database, making it available in "my messages" of
* the recipients. It also creates tasks for the recipients notifying them of the
* received message and if a reply is required creates the task for entering the reply.
*
* @param The message to be sent.
*/
sendMessage 		:: Message 								->	Task Void
/**
* Retrieve all messages of which the current user is a recipient.
*
* @return The list of messages
*/
getMyMessages		:: 											Task [Message]
/**
* Retrieve all messages stored in the system.
*
* @return The list of messages
*/
getAllMessages		::											Task [Message]