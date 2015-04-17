definition module iTasks.Framework.TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from iTasks.API.Core.Types	        import :: TaskListItem, :: TaskId, :: SessionId
from iTasks.Framework.IWorld		import :: IWorld
from iTasks.Framework.Task			import :: Task, :: TaskResult, :: Event, :: TaskEvalOpts
from iTasks.Framework.SDS           import :: Shared
from iTasks.Framework.UIDiff		import :: UIUpdate

import iTasks.Framework.TaskState, iTasks.Framework.Generic

from Text.JSON import :: JSONNode
from Data.Error import :: MaybeErrorString, :: MaybeError

/**
 * Get the next TaskId
 */
getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)

/**
* Evaluate a task instance
*
* @param The instance id
* @param An event
* @param The IWorld state
*
* @return The result of the targeted main task or an error
* @return The IWorld state
*/
evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (!EventNo,!TaskValue JSONNode,![UIUpdate]),!*IWorld)

/**
* Evaluate a task instance without any events
*
* @param The task instance number
* @param The IWorld state
*
* @return The IWorld state
*/
refreshTaskInstance :: !InstanceNo !(Maybe String) !*IWorld -> *IWorld

/**
* Evaluate a task instance without any events and restart output stream
*
* @param The task instance number
* @param The IWorld state
*
* @return The IWorld state
*/
resetTaskInstance   :: !InstanceNo !*IWorld -> *IWorld

//Update the refresh queue
queueRefresh                :: ![InstanceNo] [String]       !*IWorld -> *IWorld
queueUrgentRefresh          :: ![InstanceNo] [String]		!*IWorld -> *IWorld
dequeueRefresh              :: 								!*IWorld -> (!Maybe InstanceNo, !Maybe String, !*IWorld)

//Update the I/O information for task instances
updateInstanceLastIO        ::          ![InstanceNo]       !*IWorld -> *IWorld
updateInstanceConnect       :: !String  ![InstanceNo]       !*IWorld -> *IWorld
updateInstanceDisconnect    ::          ![InstanceNo]       !*IWorld -> *IWorld

//Shares providing access to the evaluation information (constants from an evaluation point of view)
currentInstanceShare        :: ReadOnlyShared InstanceNo
