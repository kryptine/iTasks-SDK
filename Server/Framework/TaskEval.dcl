definition module TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from SystemTypes	import :: IWorld, :: ProcessId, :: TaskInstanceMeta, :: User
from Task			import :: TaskNr, :: Task, :: TaskResult, :: Event, :: EditEvent, :: CommitEvent, :: ReversedTaskNr, :: TaskEvalFun

import Maybe, JSON, Error
import TaskContext, iTaskClass

/**
* Create a new session task instance
*
* @param The task to run as session
* @param The current session user
* @param Optional encoded data for a workflow with parameter
* @param The IWorld state
*
* @return The result of the targeted main task and the tasknr of the instance or an error
* @return The IWorld state
*/
createSessionInstance :: !(Task a) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !ProcessId), !*IWorld) |  iTask a

/**
* Evaluate a session task instance
*
* @param The session id
* @param Optionally an edit event
* @param Optionally a commit event
* @param The IWorld state
* 
* @return The result of the targeted main task or an error
* @return The IWorld state
*/
evalSessionInstance :: !ProcessId !(Maybe EditEvent) !(Maybe CommitEvent) !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)

//Helper functions: exported for use in workOn task
loadInstance	:: !ProcessId !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
editInstance	:: !(Maybe EditEvent) !TaskContext !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
evalInstance	:: !TaskNr !(Maybe CommitEvent) !TaskContext !*IWorld	-> (!MaybeErrorString (TaskResult Dynamic), !TaskContext, !*IWorld)
storeInstance	:: !TaskContext !*IWorld -> *IWorld
