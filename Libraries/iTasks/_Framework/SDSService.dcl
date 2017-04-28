definition module iTasks._Framework.SDSService

from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks._Framework.IWorld		import :: IWorld
from iTasks._Framework.WebService   import :: ConnectionState, :: WebSockState
from iTasks._Framework.SDS			import :: RWShared
from iTasks._Framework.Task			import :: Task, :: InstanceNo
from iTasks._Framework.TaskState	import :: TIUIState
from iTasks.UI.Definition           import :: UIChange
from Data.Queue						import :: Queue

import iTasks._Framework.Generic

import Data.Maybe, Data.Error, Text.JSON

sdsService ::   (!(String -> Bool)
				 ,!Bool
                 ,!(HTTPRequest (Map InstanceNo (Queue UIChange)) *IWorld -> *(!HTTPResponse, !Maybe ConnectionState, !Maybe (Map InstanceNo (Queue UIChange)), !*IWorld))
				 ,!(HTTPRequest (Map InstanceNo (Queue UIChange)) String ConnectionState *IWorld -> (![{#Char}], !Bool, !ConnectionState, !Maybe (Map InstanceNo (Queue UIChange)), !*IWorld))
				 ,!(HTTPRequest (Map InstanceNo (Queue UIChange)) ConnectionState *IWorld -> (!Maybe (Map InstanceNo (Queue UIChange)), !*IWorld))
				 )

readRemoteSDS  ::           !JSONNode !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
writeRemoteSDS :: !JSONNode !JSONNode !String !*IWorld -> *(!MaybeErrorString (),     !*IWorld)

openRemoteSDS :: !String !((Maybe (RWShared p r w)) -> Task a) -> Task a | iTask a & JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w & TC p & TC r & TC w
