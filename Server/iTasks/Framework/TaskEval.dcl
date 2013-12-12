definition module iTasks.Framework.TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from iTasks.API.Core.SystemTypes	import :: TaskListItem, :: User, :: TaskId, :: SessionId 
from iTasks.Framework.IWorld		import :: IWorld
from iTasks.Framework.Task			import :: Task, :: TaskResult, :: Event, :: TaskRepOpts
from iTasks.Framework.Shared		import :: Shared
from iTasks.Framework.UIDiff		import :: UIUpdate

import iTasks.Framework.TaskState, iTasks.Framework.Generic

from Text.JSON import :: JSONNode
from Data.Error import :: MaybeErrorString, :: MaybeError

createClientTaskInstance :: !(Task a) !SessionId !InstanceNo !*IWorld -> *(!TaskId, !*IWorld) |  iTask a

/**
* Create a new session task instance and evaluate its immediately
*
* @param The task to run as session
* @param An event
* @param The IWorld state
*
* @return The result of the targeted main task and the tasknr of the instance or an error
* @return The IWorld state
*/
createSessionTaskInstance :: !(Task a) !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !InstanceKey, !SessionInfo, ![UIUpdate]), !*IWorld) |  iTask a

//Create a session instance without evaluating it
createUnevaluatedTaskInstance :: !(Task a) !*IWorld -> (!MaybeErrorString (!InstanceNo,InstanceKey),!*IWorld) | iTask a

/**
* Create a stored task instance in the task pool (lazily without evaluating it)
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
createDetachedTaskInstance :: !(Task a) !(Maybe InstanceNo) !(Maybe String) !ManagementMeta !User !TaskId !(Maybe [TaskId]) !*IWorld -> (!TaskId, !*IWorld) | iTask a

/**
* Evaluate a session task instance
*
* @param The session id
* @param An event
* @param The IWorld state
* 
* @return The result of the targeted main task or an error
* @return The IWorld state
*/
evalSessionTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionInfo, ![UIUpdate]), !*IWorld)

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
* Refresh all urgent task instances
*
* @param The IWorld state
*
* @return The IWorld state
*/
refreshUrgentTaskInstances :: !*IWorld -> *IWorld

//Helper functions that provide access to shares and parallel task lists
localShare		:: !TaskId ->	        Shared a			| iTask a
topListShare	::				        SharedTaskList a
parListShare	:: !TaskId !TaskId ->	SharedTaskList a	| iTask a
currentInstanceShare ::                 ReadOnlyShared InstanceNo
