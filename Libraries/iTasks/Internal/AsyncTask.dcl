definition module iTasks.Internal.AsyncTask

import iTasks
from Data.Queue import :: Queue

derive JSONEncode TaskWrapper
derive JSONDecode TaskWrapper
derive gEq TaskWrapper
derive gEditor TaskWrapper
derive gText TaskWrapper

derive class iTask Queue, Event
derive gEditor Set
derive gText Set

/**
 * With the update functions, tasks can be scheduled atomically with an unique taskId
 * Only the master uses this
 */
asyncITasksQueue :: SDSLens () () (TaskId, TaskWrapper)

/**
 * Hosts the task values+uichanges over time for async tasks
 * It is a queue because no value+ui may be lost
 * The master dequeues, the follower enqueues
 */
asyncITasksValues :: SDSLens TaskId (Queue (TaskValue a, UIChange)) (Queue (TaskValue a, UIChange)) | TC, JSONEncode{|*|}, JSONDecode{|*|} a

/**
 * Hosts the incoming async tasks internally
 * The master enqueues, the follower dequeues
 */
asyncITasksQueueInt :: SimpleSDSLens (Queue (TaskId, TaskWrapper))

/**
 * Gives you a fresh task id for the given instance
 */
getNextTaskIdForInstance :: SDSSource InstanceNo TaskNo ()

/**
 * Contains the instance as provided by {{asyncTaskListener}}
 */
asyncITasksHostInstance :: SimpleSDSLens (Maybe InstanceNo)
