definition module iTasks.Framework.SDSService

from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks.Framework.IWorld		import :: IWorld
from iTasks.Framework.Engine	    import :: ConnectionType

import Data.Maybe, Data.Void, Data.Error, Text.JSON

sdsService ::   (!(String -> Bool)
				 ,!Bool
                 ,!(HTTPRequest *IWorld -> *(!HTTPResponse, !Maybe ConnectionType, !*IWorld))
				 ,!(HTTPRequest (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !*IWorld))
				 ,!(HTTPRequest ConnectionType *IWorld -> *IWorld)
				 )

readRemoteSDS  ::           !JSONNode !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
writeRemoteSDS :: !JSONNode !JSONNode !String !*IWorld -> *(!MaybeErrorString Void,     !*IWorld)
