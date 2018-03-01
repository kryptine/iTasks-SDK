definition module iTasks.Internal.SDSService

from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks.Internal.IWorld		import :: IWorld
from iTasks.Internal.WebService   import :: ConnectionState, :: WebSockState, :: WebService
from iTasks.Internal.SDS			import :: RWShared
from iTasks.Internal.Task			import :: Task, :: InstanceNo
from iTasks.Internal.TaskState	import :: TIUIState
from iTasks.UI.Definition           import :: UIChange
from Data.Queue						import :: Queue
from Data.Maybe                     import :: Maybe
from Data.Error                     import :: MaybeError, :: MaybeErrorString
from Data.Map                       import :: Map
from Text.JSON                      import :: JSONNode

from iTasks.WF.Definition import class iTask
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.Internal.Generic.Defaults import generic gDefault
from Text.JSON import generic JSONEncode, generic JSONDecode
from Data.Generics.GenEq import generic gEq

from iTasks.SDS.Definition import :: SDS

sdsService :: WebService a a

readRemoteSDS  :: !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
writeRemoteSDS :: !JSONNode !String !*IWorld -> *(!MaybeErrorString (), !*IWorld)
