definition module Messaging

import iTasks

messaging :: [Workflow]

:: Message

// Messaging
newMessage 		  ::									   Task Void
newMessageToGroup ::									   Task Void

sendMessage 	:: Message 								-> Task Void
writeMessage 	:: User String [User] [User] [Message] 	-> Task Message
readMessage 	:: Message 								-> Task Void

broadcast 		:: [User] String (Maybe a) 				-> Task Void | iTask a
