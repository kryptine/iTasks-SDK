definition module iTasks.Internal.AsyncTask

import iTasks
from Data.Queue import :: Queue

derive JSONEncode TaskWrapper, AsyncTaskResult
derive JSONDecode TaskWrapper, AsyncTaskResult
derive gEq TaskWrapper
derive gEditor TaskWrapper, Set
derive gText TaskWrapper, Set

derive class iTask Queue, Event, AsyncQueueItem

:: AsyncTaskResult a
	= AsyncTaskValue !(TaskValue a) !UIChange
	| AsyncException !TaskException

:: AsyncQueueItem
	= AsyncTaskAdd !TaskId !TaskWrapper
	| AsyncTaskRemove !TaskId

/**
 * With the update functions, tasks can be scheduled atomically with an unique taskId
 * Only the master uses this
 */
asyncITasksQueue :: SDSLens () () AsyncQueueItem

/**
 * Hosts the task values+uichanges over time for async tasks
 * It is a queue because no value+ui may be lost
 * The master dequeues, the follower enqueues
 */
asyncITasksResults :: SDSLens TaskId (Queue (AsyncTaskResult a)) (Queue (AsyncTaskResult a)) | TC, JSONEncode{|*|}, JSONDecode{|*|} a

/**
 * Hosts the incoming async tasks and to be removed tasks internally
 * The master enqueues, the follower dequeues
 */
asyncITasksQueueInt :: SimpleSDSLens (Queue AsyncQueueItem)

/**
 * Gives you a fresh task id for the given instance
 */
getNextTaskIdForInstance :: SDSSource InstanceNo TaskNo ()

/**
 * Contains the instance as provided by {{asyncTaskListener}}
 */
asyncITasksHostInstance :: SimpleSDSLens (Maybe InstanceNo)
