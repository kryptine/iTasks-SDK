definition module iTasks._Framework.SDSService

from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks._Framework.IWorld		import :: IWorld
from iTasks._Framework.WebService   import :: ConnectionState, :: WebSockState, :: WebService
from iTasks._Framework.SDS			import :: RWShared
from iTasks._Framework.Task			import :: Task, :: InstanceNo
from iTasks._Framework.TaskState	import :: TIUIState
from iTasks.UI.Definition           import :: UIChange
from Data.Queue						import :: Queue
from Data.Maybe                     import :: Maybe
from Data.Error                     import :: MaybeError, :: MaybeErrorString
from Data.Map                       import :: Map
from Text.JSON                      import :: JSONNode

from iTasks.WF.Definition import class iTask
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks._Framework.Generic.Visualization import generic gText, :: TextFormat
from iTasks._Framework.Generic.Defaults import generic gDefault
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq

from iTasks.SDS.Definition import :: SDS

sdsService :: WebService (Map InstanceNo (Queue UIChange)) (Map InstanceNo (Queue UIChange))

readRemoteSDS  ::           !JSONNode !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
writeRemoteSDS :: !JSONNode !JSONNode !String !*IWorld -> *(!MaybeErrorString (),     !*IWorld)

openRemoteSDS :: !String !((Maybe (RWShared p r w)) -> Task a) -> Task a | iTask a & JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w & TC p & TC r & TC w
