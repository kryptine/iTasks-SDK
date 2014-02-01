definition module iTasks.Framework.WebService
/**
* This module provides the web service that gives access to tasks via the web.
* It also provides access to upload/download of blob content.
*/
from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks.Framework.Engine		import :: ServiceFormat, :: ConnectionType
from iTasks.Framework.IWorld		import :: IWorld
from iTasks.Framework.Task 			import :: Task, :: ConnectionTask
from iTasks.API.Core.Types	        import :: InstanceNo

import iTasks.Framework.Generic

httpServer :: !Int !Int ![(!String -> Bool
				,!Bool
				,!(HTTPRequest *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !*IWorld))
				,!(HTTPRequest (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !*IWorld))
				,!(HTTPRequest ConnectionType *IWorld -> *IWorld)
				)] -> ConnectionTask

webService :: !String !(HTTPRequest -> Task a) !ServiceFormat ->
				 (!(String -> Bool)
                 ,!(HTTPRequest *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !*IWorld))
				 ,!(HTTPRequest (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !*IWorld))
				 ,!(HTTPRequest ConnectionType *IWorld -> *IWorld)
				 ) | iTask a
