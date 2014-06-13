definition module iTasks.Framework.TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from iTasks.API.Core.Types	        import :: TaskListItem, :: User, :: TaskId, :: SessionId 
from iTasks.Framework.IWorld		import :: IWorld
from iTasks.Framework.Task			import :: Task, :: TaskResult, :: Event, :: TaskRepOpts
from iTasks.Framework.SDS           import :: Shared
from iTasks.Framework.UIDiff		import :: UIUpdate

import iTasks.Framework.TaskState, iTasks.Framework.Generic

from Text.JSON import :: JSONNode
from Data.Error import :: MaybeErrorString, :: MaybeError

createClientTaskInstance :: !(Task a) !SessionId !InstanceNo !*IWorld -> *(!TaskId, !*IWorld) |  iTask a

//Create a task instance
createTaskInstance :: !(Task a) !*IWorld -> (!MaybeErrorString (!InstanceNo,InstanceKey),!*IWorld) | iTask a

/**
* Create a stored task instance in the task store (lazily without evaluating it)
* @param The task to store
* @param Management meta data
* @param The user who issued the task
* @param The parent instance that created the instance
* @param If the instance needs to be evaluated immediately, the attachment of the task that created the instance
* @param The IWorld state
*
* @return The task id of the stored instance
* @return The IWorld state
*/
createDetachedTaskInstance :: !(Task a) !(Maybe InstanceNo) !(Maybe String) !TaskAttributes !User !TaskId !(Maybe [TaskId]) !*IWorld -> (!TaskId, !*IWorld) | iTask a

/**
* Replace a stored task instance in the task store.
* The execution state is reset, but the meta-data is kept.
* @param The instance id
* @param The new task to store
*
* @param The IWorld state
*/
replaceTaskInstance :: !InstanceNo !(Task a) *IWorld -> (!MaybeErrorString (), !*IWorld) | iTask a

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
refreshTaskInstance :: !InstanceNo !*IWorld -> *IWorld

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
queueRefresh        :: ![InstanceNo]                !*IWorld -> *IWorld
queueUrgentRefresh  :: ![InstanceNo]				!*IWorld -> *IWorld
dequeueRefresh      :: 								!*IWorld -> (!Maybe InstanceNo, !*IWorld)

//Update the I/O information for task instances
updateInstanceLastIO        ::          ![InstanceNo]       !*IWorld -> *IWorld
updateInstanceConnect       :: !String  ![InstanceNo]       !*IWorld -> *IWorld
updateInstanceDisconnect    ::          ![InstanceNo]       !*IWorld -> *IWorld

//Helper functions that provide access to shares and parallel task lists
localShare		        :: !TaskId ->	        Shared a			| iTask a
exposedShare 	        :: !String -> 			RWShared p r w	    | iTask r & iTask w & TC r & TC w & TC p & JSONEncode{|*|} p
topListShare	        ::				        SharedTaskList a
parListShare	        :: !TaskId !TaskId ->	SharedTaskList a	| iTask a
currentInstanceShare    ::                 ReadOnlyShared InstanceNo
