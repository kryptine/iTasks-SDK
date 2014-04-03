definition module iTasks.Framework.SDSService

from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks.Framework.IWorld		import :: IWorld
from iTasks.Framework.Engine	    import :: ConnectionType
from iTasks.Framework.SDS			import :: RWShared
from iTasks.Framework.Task			import :: Task

import iTasks.Framework.Generic

import Data.Maybe, Data.Void, Data.Error, Text.JSON

sdsService ::   (!(String -> Bool)
				 ,!Bool
                 ,!(HTTPRequest *IWorld -> *(!HTTPResponse, !Maybe ConnectionType, !*IWorld))
				 ,!(HTTPRequest (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !*IWorld))
				 ,!(HTTPRequest ConnectionType *IWorld -> *IWorld)
				 )

readRemoteSDS  ::           !JSONNode !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
writeRemoteSDS :: !JSONNode !JSONNode !String !*IWorld -> *(!MaybeErrorString Void,     !*IWorld)

openRemoteSDS :: !String !((Maybe (RWShared p r w)) -> Task a) -> Task a | iTask a & JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w & TC p & TC r & TC w