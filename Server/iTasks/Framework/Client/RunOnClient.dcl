definition module iTasks.Framework.Client.RunOnClient

import iTasks

runOnClient :: !(Task m) -> Task m | iTask m
createClientIWorld :: !InstanceNo -> *IWorld
getUIUpdates :: !*IWorld -> (!Maybe [(InstanceNo, [String])], *IWorld)