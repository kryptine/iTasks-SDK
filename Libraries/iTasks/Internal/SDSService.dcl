definition module iTasks.Internal.SDSService

from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks.Internal.IWorld		import :: IWorld
from iTasks.Internal.WebService   import :: ConnectionState, :: WebSockState, :: WebService
import iTasks.SDS.Definition
from iTasks.Internal.Task			import :: Task, :: InstanceNo
from iTasks.Internal.TaskState	import :: TIUIState
from iTasks.UI.Definition           import :: UIChange
from Data.Queue						import :: Queue
from Data.Maybe                     import :: Maybe
from Data.Error                     import :: MaybeError, :: MaybeErrorString
from Data.Map                       import :: Map
from Text.GenJSON                      import :: JSONNode

from iTasks.WF.Definition import class iTask
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.Internal.Generic.Defaults import generic gDefault
from Text.GenJSON import generic JSONEncode, generic JSONDecode
from Data.GenEq import generic gEq

sdsService :: WebService a a

writeRemoteSDS :: !JSONNode !JSONNode !String !*IWorld -> *(!MaybeErrorString (),     !*IWorld)
