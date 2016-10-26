definition module iTasks._Framework.WebService
/**
* This module provides the web service that gives access to tasks via the web.
* It also provides access to upload/download of blob content.
*/
from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks._Framework.IWorld		import :: IWorld
from iTasks._Framework.Task 	    import :: Task, :: ConnectionTask
from iTasks._Framework.TaskState 	import :: TIUIState
from iTasks._Framework.SDS 			import :: RWShared
from iTasks.UI.Definition           import :: UIChange
from iTasks.API.Core.Types	        import :: InstanceNo
from Data.Queue 					import :: Queue

import iTasks._Framework.Generic

//Connection types used by the engine
:: ConnectionType
    = EventSourceConnection [InstanceNo]            //Server -> Client updates push
    | WebSocketConnection WebSockState [InstanceNo] //Server <-> Client events and updates channel

:: WebSockState =
	{ cur_frame    :: !{#Char}   //The fram
	, message_text :: !Bool     // True -> text message, False -> binary
	, message_data :: ![String] // Message data from previous frames 
	}

:: WebSockEvent
	= WSTextMessage String //A UTF-8 text message was received completely
	| WSBinMessage String  //A binary message was received completely
	| WSClose String       //A close frame was received
	| WSPing String        //A ping frame was received

httpServer :: !Int !Int ![(!String -> Bool
				,!Bool
				,!(HTTPRequest r *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !Maybe w, !*IWorld))
				,!(HTTPRequest r (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !Maybe w, !*IWorld))
				,!(HTTPRequest r ConnectionType *IWorld -> (!Maybe w, !*IWorld))
				)] (RWShared () r w) -> ConnectionTask | TC r & TC w


:: ChangeQueues :== Map InstanceNo (Queue UIChange)

taskWebService :: !String !(HTTPRequest -> Task a) ->
                 (!(String -> Bool)
				 ,!Bool
                 ,!(HTTPRequest ChangeQueues *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !Maybe ChangeQueues, !*IWorld))
                 ,!(HTTPRequest ChangeQueues (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !Maybe ChangeQueues, !*IWorld))
                 ,!(HTTPRequest ChangeQueues ConnectionType *IWorld -> (!Maybe ChangeQueues, !*IWorld))
                 ) | iTask a

staticResourceService :: [String] ->
                 (!(String -> Bool)
				 ,!Bool
                 ,!(HTTPRequest r *IWorld -> (HTTPResponse, Maybe loc, Maybe w ,*IWorld))
				 ,!(HTTPRequest r (Maybe {#Char}) loc *IWorld -> (![{#Char}], !Bool, loc, Maybe w ,!*IWorld))
				 ,!(HTTPRequest r loc *IWorld -> (!Maybe w,!*IWorld))
                 )
