implementation module iTasks.Extensions.Device.Features

import iTasks
import iTasks.Internal.SDS

import StdString

derive class iTask DeviceFeatures

hasCamera :: DeviceFeatures -> Bool
hasCamera {DeviceFeatures|camera} = camera

device :: SimpleSDSLens DeviceFeatures
device = sharedStore "deviceFeaturs" {DeviceFeatures| camera = False }

manageDeviceFeaturs :: Task DeviceFeatures
manageDeviceFeaturs
	=              get device
	>>- \info -> Hint "Manage device features" @>> updateInformation [] info
	>>= \info -> set info device
