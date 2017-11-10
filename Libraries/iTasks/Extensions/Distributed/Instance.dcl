definition module iTasks.Extensions.Distributed.Instance

import iTasks
import iTasks.Extensions.Distributed._Formatter
from iTasks.Extensions.Distributed.Task import :: Domain

instanceServer :: Int Domain -> Task ()

instanceClient :: String Int Domain -> Task ()

instanceFilter :: (TaskAttributes -> Bool) Domain -> Task ()

instanceClameFilter :: (TaskAttributes -> Bool) Domain -> Task ()

sendDistributedInstance :: InstanceNo (Task a) TaskAttributes Domain -> Task a | iTask a

sendRequestToInstanceServer :: Int String -> Task ()
