implementation module DynamicIOTasks

import StdDynamic
import StdFile
import StdMisc

import iTasks
from Serialization import qualified serialize, deserialize

instance toString DynamicIOException
where
	toString (DynamicIOException errorString) = errorString

writeDynamicTask :: !String !(Task a) -> Task Void | iTask a
writeDynamicTask filename task 
	= exportTextFile filename ('Serialization'.serialize task) >>| stop

readDynamicTask :: !String -> Task (Task a) | iTask a
readDynamicTask filename = importTextFile filename >>= \dynString -> 
	case 'Serialization'.deserialize dynString of
		Ok value = return value
		Error errorString = throw (DynamicIOException errorString)
