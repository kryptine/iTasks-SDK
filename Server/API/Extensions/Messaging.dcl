definition module Messaging

import iTasks

:: Message =
	{ messageId			:: Hidden Int
	, sender			:: HtmlDisplay User
	, recipients 		:: [User]
	, cc 				:: Maybe [User]
	, priority 			:: TaskPriority
	, subject			:: String
	, message			:: Note
	, attachments		:: Maybe [Document]
	, previousMessages 	:: HtmlDisplay [Message]
	}

manageMessages		:: 											Task Void

newMessage 		  	::											Task Void
newGroupMessage		::											Task Void

sendMessage 		:: Message 								->	Task Void
writeMessage 		:: User String [User] [User] [Message] 	->	Task Message
readMessage 		:: Message 								->	Task Void

getMyMessages		:: 											Task [Message]