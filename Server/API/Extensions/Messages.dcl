definition module Messages

import iTasks

:: Message =
	{ messageId			:: Hidden Int
	, sender			:: Display User
	, recipients 		:: [User]
	, cc 				:: Maybe [User]
	, priority 			:: TaskPriority
	, subject			:: String
	, message			:: Note
	, attachments		:: Maybe [Document]
	, previousMessages 	:: Display [Message]
	}

manageMessages		:: 											Task Void

newMessage 		  	::											Task Void
newGroupMessage		::											Task Void

sendMessage 		:: Message 								->	Task Void
writeMessage 		:: User String [User] [User] [Message] 	->	Task Message
readMessage 		:: Message 								->	Task Void

getMyMessages		:: 											Task [Message]