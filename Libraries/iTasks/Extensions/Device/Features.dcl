definition module iTasks.Extensions.Device.Features

import iTasks
//from iTasks._Framework.Generic import class iTask
//from iTasks._Framework.SDS import :: RWShared
//from iTasks.API.Core.Types import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
//from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
//from Data.Maybe import :: Maybe
//from iTasks._Framework.Generic.Visualization    import :: TextFormat(..)

:: DeviceFeatures = { camera :: Bool }

derive class iTask DeviceFeatures

hasCamera :: DeviceFeatures -> Bool

device :: RWShared () DeviceFeatures DeviceFeatures

manageDeviceFeaturs :: Task DeviceFeatures
