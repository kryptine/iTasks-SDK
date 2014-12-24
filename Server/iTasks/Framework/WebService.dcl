definition module iTasks.Framework.WebService
/**
* This module provides the web service that gives access to tasks via the web.
* It also provides access to upload/download of blob content.
*/
from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks.Framework.Engine		import :: ServiceFormat, :: ConnectionType
from iTasks.Framework.IWorld		import :: IWorld
from iTasks.Framework.Task 			import :: Task, :: ConnectionTask
from iTasks.Framework.UIDiff 		import :: UIUpdate
from iTasks.API.Core.Types	        import :: InstanceNo
from iTasks.Framework.SDS 			import :: RWShared

import iTasks.Framework.Generic

httpServer :: !Int !Int ![(!String -> Bool
				,!Bool
				,!(HTTPRequest r *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !*IWorld))
				,!(HTTPRequest r (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !*IWorld))
				,!(HTTPRequest r ConnectionType *IWorld -> *IWorld)
				)] (RWShared () r w) -> ConnectionTask | TC r & TC w

webService :: !String !(HTTPRequest -> Task a) !ServiceFormat ->
                 (!(String -> Bool)
                 ,!(HTTPRequest (Map InstanceNo [UIUpdate]) *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !*IWorld))
                 ,!(HTTPRequest (Map InstanceNo [UIUpdate]) (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !*IWorld))
                 ,!(HTTPRequest (Map InstanceNo [UIUpdate]) ConnectionType *IWorld -> *IWorld)
                 ) | iTask a
