definition module TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from SystemTypes	import :: IWorld, :: TaskListItem, :: User, :: TaskId, :: SessionId 
from Task			import :: Task, :: TaskResult, :: Event, :: TaskRepOpts
from Shared			import :: Shared

import Maybe, JSON, Error
import TaskState, iTaskClass

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
createSessionTaskInstance :: !(Task a) !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionId), !*IWorld) |  iTask a

/**
* Create a stored task instance in the task pool (lazily without evaluating it)
* @param The task to store
* @param Management meta data
* @param The user who issued the task
* @param The parent instance that created the instance
* @param The IWorld state
*
* @return The task id of the stored instance
* @return The IWorld state
*/
createTopTaskInstance :: !(Task a) !ManagementMeta !User !InstanceNo !*IWorld -> (!TaskId, !*IWorld) | iTask a

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
evalSessionTaskInstance :: !SessionId !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionId), !*IWorld)

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
localShare		:: !TaskId ->	Shared a			| iTask a
topListShare	::				SharedTaskList a
parListShare	:: !TaskId ->	SharedTaskList a	| iTask a
