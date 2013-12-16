definition module iTasks.Framework.SDSService

from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks.Framework.IWorld		import :: IWorld
from iTasks.API.Core.SystemTypes	import :: InstanceNo

import Data.Maybe, Data.Void, Data.Error, Text.JSON

sdsService ::   (!(String -> Bool)
				 ,!Bool
                 ,!(HTTPRequest *IWorld -> *(!HTTPResponse, !Maybe InstanceNo, !*IWorld))
				 ,!(HTTPRequest (Maybe {#Char}) InstanceNo *IWorld -> (!Maybe {#Char}, !Bool, !InstanceNo, !*IWorld))
				 ,!(HTTPRequest InstanceNo *IWorld -> *IWorld)
				 )

readRemoteSDS  :: 			!String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
writeRemoteSDS :: !JSONNode !String !*IWorld -> *(!MaybeErrorString Void,     !*IWorld)