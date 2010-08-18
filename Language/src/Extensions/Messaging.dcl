definition module Messaging

import iTasks

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

// Messaging
newMessage 		  	::											Task Void
newMessageToGroup 	::											Task Void
viewArchive			:: 											Task Void

sendMessage 		:: Message 								-> Task Void
writeMessage 		:: User String [User] [User] [Message] 	-> Task Message
readMessage 		:: Message 								-> Task Void

broadcast 			:: [User] String (Maybe a) 				-> Task Void | iTask a
