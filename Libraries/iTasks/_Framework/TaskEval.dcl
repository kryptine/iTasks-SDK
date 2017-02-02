definition module iTasks._Framework.TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from iTasks.API.Core.Types	        import :: TaskListItem, :: TaskId, :: SessionId
from iTasks._Framework.IWorld		import :: IWorld
from iTasks._Framework.Task			import :: Task, :: TaskResult, :: Event, :: TaskEvalOpts
from iTasks._Framework.SDS          import :: Shared

import iTasks._Framework.TaskState, iTasks._Framework.Generic

from Text.JSON import :: JSONNode
from Data.Error import :: MaybeErrorString, :: MaybeError

/**
 * Get the next TaskId
 */
getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
/**
* Dequeues events from the event queue and evaluates the tasks instances
* @param Maximum amount of events to process at once
*/
processEvents :: !Int *IWorld -> *(!MaybeError TaskException (), !*IWorld)

/**
* Evaluate a task instance
*
* @param The instance id
* @param The event to process
* @param The IWorld state
*
* @return The result of the targeted main task or an error
* @return The IWorld state
*/
evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (TaskValue JSONNode),!*IWorld)

//Update the I/O information for task instances
updateInstanceLastIO        ::          ![InstanceNo]       !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceConnect       :: !String  ![InstanceNo]       !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceDisconnect    ::          ![InstanceNo]       !*IWorld -> *(!MaybeError TaskException (), !*IWorld)

//Shares providing access to the evaluation information (constants from an evaluation point of view)
currentInstanceShare        :: ReadOnlyShared InstanceNo
