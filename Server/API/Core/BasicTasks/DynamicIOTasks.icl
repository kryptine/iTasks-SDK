implementation module DynamicIOTasks

import StdDynamic
import StdFile
import StdMisc

import iTasks
from Serialization import qualified serialize, deserialize

writeDynamicTask :: !String !(Task a) -> Task Void | iTask a
writeDynamicTask filename task 
	= exportTextFile filename ('Serialization'.serialize task) >>| stop

readDynamicTask :: !String -> Task (Maybe (Task a)) | iTask a
readDynamicTask filename = importTextFile filename >>= transform 'Serialization'.deserialize 
