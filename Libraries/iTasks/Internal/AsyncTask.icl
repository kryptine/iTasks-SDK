implementation module iTasks.Internal.AsyncTask

import iTasks
import Data.Queue, Data.Tuple, Data.Functor
import iTasks.Internal.Serialization
import iTasks.Internal.TaskEval
import iTasks.Internal.TaskState
import iTasks.UI.Editor.Common
import qualified Data.Set
from Data.Set import class Foldable, instance Foldable Set
import Data.Set.GenJSON

JSONEncode{|TaskWrapper|} _ t = [dynamicJSONEncode t]
JSONDecode{|TaskWrapper|} _ [t:c] = (dynamicJSONDecode t, c)
JSONDecode{|TaskWrapper|} _ c = (Nothing, c)
gEq{|TaskWrapper|} _ _ = False
gEditor{|TaskWrapper|} = emptyEditor
gText{|TaskWrapper|} tf ma = maybe [] (\_->["TaskWrapper"]) ma

derive class iTask Queue, Event
derive gEditor Set
gText{|Set|} m tf ms = gText{|*->*|} m tf ('Data.Set'.toList <$> ms)

asyncITasksQueue :: SDSLens () () (TaskId, TaskWrapper)
asyncITasksQueue = mapReadWrite (\_->(), \task queue->Just (enqueue task queue)) Nothing asyncITasksQueueInt

asyncITasksValues :: SDSLens TaskId (Queue (TaskValue a, UIChange)) (Queue (TaskValue a, UIChange)) | TC, JSONEncode{|*|}, JSONDecode{|*|} a
asyncITasksValues = sdsTranslate "taskIdToString" toString (memoryStore "asyncITasks-values" (Just newQueue))

asyncITasksQueueInt :: SimpleSDSLens (Queue (TaskId, TaskWrapper))
asyncITasksQueueInt = sdsFocus "queue" (memoryStore "asyncITasks" (Just newQueue))

getNextTaskIdForInstance :: SDSSource InstanceNo TaskNo ()
getNextTaskIdForInstance = SDSSource
	{ SDSSourceOptions
	| name = "getNextTaskIdForInstance"
	, read = \instanceNo iworld
		# (merr, iworld) = read (sdsFocus (instanceNo,False,False) taskInstance) EmptyContext iworld
		| isError merr = (Error (fromError merr), iworld)
		# (ReadingDone meta=:{TaskMeta|nextTaskNo}) = fromOk merr
		# (merr,iworld) = write {meta & nextTaskNo=nextTaskNo+1} (sdsFocus (instanceNo,False,False) taskInstance) EmptyContext iworld
		| isError merr = (Error (fromError merr), iworld)
		= (Ok nextTaskNo, iworld)
	, write = \_ _ iworld->(Ok \_ _->False, iworld)
	}

asyncITasksHostInstance :: SimpleSDSLens (Maybe InstanceNo)
asyncITasksHostInstance = sdsFocus "instance" (memoryStore "asyncITasks" (Just Nothing))
