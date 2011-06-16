definition module TaskInstance
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from SystemTypes	import :: IWorld, :: WorkflowId, :: ProcessId, :: ProcessProperties, :: User
from Task			import :: TaskNr, :: Task, :: TaskResult, :: EditEvent, :: CommitEvent, :: ReversedTaskNr

import Maybe, JSON, Error
import iTaskClass

/**
* Creates a dynamic containing a runnable task thread structure.
* It contains the task plus the iTask context restrictions.
*
* @param The workflow title (for param only)
* @param The task that is to be converted to a runnable thread (with ot without parameter)
* 
* @return A dynamic containing the thread
*/
createThread		:: (Task a)					-> Dynamic	| iTask a
createThreadParam	:: !String (a -> Task b)	-> Dynamic	| iTask a & iTask b

/**
* Create a new instance (process) of a workflow in the workflow database
*/
createWorkflowInstance :: !WorkflowId !User !(Maybe JSONNode) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic,!ProcessProperties), !*IWorld)
/**
* Evaluate an existing workflow instance.
*/
evaluateWorkflowInstance :: !ProcessId !(Maybe EditEvent) !(Maybe CommitEvent) !TaskNr !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic,!ProcessProperties), !*IWorld)