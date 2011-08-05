definition module TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from SystemTypes	import :: IWorld, :: WorkflowId, :: ProcessId, :: ProcessProperties, :: User
from Task			import :: TaskNr, :: Task, :: TaskResult, :: Event, :: EditEvent, :: CommitEvent, :: ReversedTaskNr, :: TaskEvalFun

import Maybe, JSON, Error
import TaskContext, iTaskClass


/**
* Create a new top-level task instance
*
* @param The name of the workflow
* @param The current session user
* @param Optional encoded data for a workflow with parameter
* @param The IWorld state
*
* @return The result of the targeted main task and the tasknr of the instance or an error
* @return The IWorld state
*/
createTopInstance :: !WorkflowId !User !(Maybe JSONNode) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !TaskNr), !*IWorld)

/**
* Evaluate a top-level task instance
*
* @param The task number of the main/detached task which result is to be returned
* @param The current session user
* @param Optionally an edit event
* @param Optionally a commit event
* @param The IWorld state
* 
* @return The result of the targeted main task or an error
* @return The IWorld state
*/
evalTopInstance :: !TaskNr !User !(Maybe EditEvent) !(Maybe CommitEvent) !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)


//Helper functions: exported for use in workOn task
loadInstance	:: !TaskNr !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
editInstance	:: !(Maybe EditEvent) !TaskContext !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
evalInstance	:: !TaskNr !(Maybe CommitEvent) !TaskContext  !*IWorld	-> (!MaybeErrorString (TaskResult Dynamic), !TaskContext, !*IWorld)
storeInstance	:: !TaskContext !*IWorld -> *IWorld
