definition module TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from SystemTypes	import :: IWorld, :: TaskListItem, :: User, :: TaskId, :: SessionId 
from Task			import :: Task, :: TaskResult, :: Event, :: EditEvent, :: CommitEvent, :: TaskEvalFun, :: TaskRepTarget

import Maybe, JSON, Error
import TaskContext, iTaskClass

/**
* Create a new session task instance
*
* @param The task to run as session
* @param Generate a GUI
* @param The IWorld state
*
* @return The result of the targeted main task and the tasknr of the instance or an error
* @return The IWorld state
*/
createSessionInstance :: !(Task a) !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !SessionId), !*IWorld) |  iTask a

/**
* Evaluate a session task instance
*
* @param The session id
* @param Optionally an edit event
* @param Optionally a commit event
* @param Generate a GUI
* @param The IWorld state
* 
* @return The result of the targeted main task or an error
* @return The IWorld state
*/
evalSessionInstance :: !SessionId !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !SessionId), !*IWorld)

//Helper functions: exported for use in workOn and parallel
evalInstance	:: !(Maybe EditEvent) !(Maybe CommitEvent) !(Maybe TaskId) !Bool !TopInstance !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !TopInstance, !*IWorld)

//Access to shared parallel information
taskListShare	:: !(TaskListId a) -> (SharedTaskList a) | iTask a