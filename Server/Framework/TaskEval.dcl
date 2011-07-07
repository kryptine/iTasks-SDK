definition module TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from SystemTypes	import :: IWorld, :: WorkflowId, :: ProcessId, :: ProcessProperties, :: User
from Task			import :: TaskNr, :: Task, :: TaskResult, :: EditEvent, :: CommitEvent, :: ReversedTaskNr, :: TaskEvalFun

import Maybe, JSON, Error
import TaskContext, iTaskClass

/**
* Creates a dynamic containing a runnable task thread structure.
* It contains the task plus the iTask context restrictions.
*
* @param The task that is to be converted to a runnable thread
* 
* @return A dynamic containing the thread
*/
createThread :: (Task a) -> Dynamic | iTask a

//Creeer initiele task context
makeWorkflowInstance	:: !WorkflowId !User !(Maybe JSONNode) !*IWorld	-> (!MaybeErrorString TaskContext, !*IWorld)

//Laadt bestaande context en pas eventuele edit events toe
loadWorkflowInstance	:: !TaskNr !(Maybe EditEvent) !*IWorld					-> (!MaybeErrorString TaskContext, !*IWorld)

//Evalueer de task in de gegeven context
evalWorkflowInstance	:: !TaskNr !TaskContext !(Maybe CommitEvent) !*IWorld	-> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)

/**
* Create a new instance (process) of a workflow in the workflow database
*/
createWorkflowInstance :: !WorkflowId !User !(Maybe JSONNode) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic,!ProcessProperties), !*IWorld)
/**
* Evaluate an existing workflow instance.
*/
evaluateWorkflowInstance :: !ProcessId !(Maybe EditEvent) !(Maybe CommitEvent) !TaskNr !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
/**
* Performs the evaluation pass of an an existing workflow instance.
*/
evaluateWorkflowInstanceEval :: !ProcessId !TaskProperties !Int !TaskNr !ProcessProperties !Dynamic !TaskContextTree !(Maybe CommitEvent) !TaskNr !*IWorld -> (!TaskResult Dynamic, !*IWorld)