definition module iTasks.Framework.WebService
/**
* This module provides the web service that gives access to tasks via the web.
* It also provides access to upload/download of blob content.
*/
from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks.Framework.Engine		import :: ServiceFormat
from iTasks.Framework.IWorld		import :: IWorld
from iTasks.Framework.Task 			import :: Task
from iTasks.API.Core.SystemTypes	import :: InstanceNo

import iTasks.Framework.Generic

webService :: !String !(HTTPRequest -> Task a) !ServiceFormat ->
				 (!(String -> Bool)
                 ,!(HTTPRequest *IWorld -> (!HTTPResponse,!Maybe InstanceNo, !*IWorld))
				 ,!(HTTPRequest (Maybe {#Char}) InstanceNo *IWorld -> (!Maybe {#Char}, !Bool, !InstanceNo, !*IWorld))
				 ,!(HTTPRequest InstanceNo *IWorld -> *IWorld)
				 ) | iTask a
