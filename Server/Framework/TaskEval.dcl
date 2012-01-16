definition module TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from SystemTypes	import :: IWorld, :: ProcessId, :: TaskInstanceMeta, :: User
from Task			import :: TaskNr, :: Task, :: TaskResult, :: Event, :: EditEvent, :: CommitEvent, :: ReversedTaskNr, :: TaskEvalFun, :: TaskRepTarget

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
createSessionInstance :: !(Task a) !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !ProcessId), !*IWorld) |  iTask a

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
evalSessionInstance :: !ProcessId !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (TaskResult Dynamic, !ProcessId), !*IWorld)

//Helper functions: exported for use in workOn task
editInstance	:: !(Maybe EditEvent) !TaskContext !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
evalInstance	:: !TaskNr !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !TaskContext !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !TaskContext, !*IWorld)
