definition module iTasks.Framework.WebService
/**
* This module provides the web service that gives access to tasks via the web.
* It also provides access to upload/download of blob content.
*/
from Internet.HTTP	import :: HTTPRequest, :: HTTPResponse
from iTasks.Framework.Engine	import :: ServiceFormat
from iTasks.Framework.IWorld	import :: IWorld
import iTasks.Framework.iTaskClass

webService :: !(HTTPRequest -> Task a) !ServiceFormat ->
				 (!(HTTPRequest *IWorld -> (!HTTPResponse,!Maybe SessionId, !*IWorld))
				 ,!(HTTPRequest (Maybe {#Char}) SessionId *IWorld -> (!Maybe {#Char}, !Bool, !SessionId, !*IWorld))
				 ,!(HTTPRequest SessionId *IWorld -> *IWorld)
				 ) | iTask a
