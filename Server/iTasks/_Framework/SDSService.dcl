definition module iTasks._Framework.SDSService

from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks._Framework.IWorld		import :: IWorld
from iTasks._Framework.Engine	    import :: ConnectionType
from iTasks._Framework.SDS			import :: RWShared
from iTasks._Framework.Task			import :: Task, :: InstanceNo
from iTasks._Framework.UIDiff 		import :: UIUpdate

import iTasks._Framework.Generic

import Data.Maybe, Data.Void, Data.Error, Text.JSON

sdsService ::   (!(String -> Bool)
				 ,!Bool
                 ,!(HTTPRequest (Map InstanceNo [UIUpdate]) *IWorld -> *(!HTTPResponse, !Maybe ConnectionType, !Maybe (Map InstanceNo [UIUpdate]), !*IWorld))
				 ,!(HTTPRequest (Map InstanceNo [UIUpdate]) (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !Maybe (Map InstanceNo [UIUpdate]), !*IWorld))
				 ,!(HTTPRequest (Map InstanceNo [UIUpdate]) ConnectionType *IWorld -> (!Maybe (Map InstanceNo [UIUpdate]), !*IWorld))
				 )

readRemoteSDS  ::           !JSONNode !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
writeRemoteSDS :: !JSONNode !JSONNode !String !*IWorld -> *(!MaybeErrorString Void,     !*IWorld)

openRemoteSDS :: !String !((Maybe (RWShared p r w)) -> Task a) -> Task a | iTask a & JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w & TC p & TC r & TC w