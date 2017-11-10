definition module iTasks.API.Extensions.Distributed.Instance

import iTasks
import iTasks.API.Extensions.Distributed._Formatter
from iTasks.API.Extensions.Distributed.Task import :: Domain

instanceServer :: Int Domain -> Task ()

instanceClient :: String Int Domain -> Task ()

instanceFilter :: (TaskAttributes -> Bool) Domain -> Task ()

instanceClameFilter :: (TaskAttributes -> Bool) Domain -> Task ()

sendDistributedInstance :: InstanceNo (Task a) TaskAttributes Domain -> Task a | iTask a

sendRequestToInstanceServer :: Int String -> Task ()
