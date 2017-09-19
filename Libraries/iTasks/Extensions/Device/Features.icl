implementation module iTasks.Extensions.Device.Features

from iTasks.API.Core.SDSs import sharedStore
from iTasks._Framework.SDS import :: RWShared
from iTasks.API.Core.Tasks import get, set
from iTasks._Framework.Generic import class iTask
from iTasks.API.Core.Types import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from Data.Maybe import :: Maybe
from iTasks._Framework.Generic.Visualization    import :: TextFormat(..)
from iTasks._Framework.SDS import :: ReadWriteShared, :: Shared
from iTasks.API.Common.InteractionTasks import :: UpdateOption(..), updateInformation
import iTasks.API.Common.TaskCombinators
from iTasks.UI.Prompt import instance toPrompt String

derive class iTask DeviceFeatures

hasCamera :: DeviceFeatures -> Bool
hasCamera {DeviceFeatures|camera} = camera

device :: RWShared () DeviceFeatures DeviceFeatures
device = sharedStore "deviceFeaturs" {DeviceFeatures| camera = False }

manageDeviceFeaturs :: Task DeviceFeatures
manageDeviceFeaturs
	=              get device
	>>- \info -> updateInformation "Manage device featurs" [] info
	>>= \info -> set info device
