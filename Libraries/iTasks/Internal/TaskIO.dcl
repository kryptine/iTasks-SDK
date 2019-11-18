definition module iTasks.Internal.TaskIO
/**
* This module provides the event and message queues that are used to communicate with tasks instances.
*/

from iTasks.Internal.IWorld import :: IWorld
from iTasks.UI.Definition import :: UIChange
from iTasks.WF.Definition import :: Task, :: TaskResult, :: TaskValue, :: TaskException, :: TaskNo, :: TaskId, :: TaskAttributes, :: TaskEvalOpts, :: Event
from iTasks.SDS.Definition import :: SimpleSDSLens, :: SDSLens, :: SDSSequence
from iTasks.WF.Definition import :: InstanceNo, :: InstanceKey, :: InstanceProgress
from Data.GenEq import generic gEq
from Data.Error import :: MaybeError
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Data.Queue import :: Queue
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

//When events are placed in this queue, the engine will re-evaluate the corresponding task instances.
:: TaskInput :== Queue (InstanceNo,Event)

taskEvents :: SimpleSDSLens TaskInput

//When task instances are evaluated, their output consists of instructions to modify the user interface
//of that instance to reflect the instance's new state

:: TaskOutputMessage
	= TOUIChange !UIChange
	| TOException !String
	| TODetach !InstanceNo

:: TaskOutput :== Queue TaskOutputMessage

taskOutput :: SimpleSDSLens (Map InstanceNo TaskOutput)
taskInstanceOutput	:: SDSLens InstanceNo TaskOutput TaskOutput

/**
* Queue an event for a task instance
* events are applied in FIFO order when the task instance is evaluated
*
* By splitting up event queuing and instance evaluation, events can come in asynchronously without
* the need to directly processing them.
*/
queueEvent :: !InstanceNo !Event !*IWorld -> *IWorld

/**
* Convenience function for queueing multiple refresh multiple refresh events at once
*/
queueRefresh :: ![(TaskId, String)] !*IWorld -> *IWorld

/**
* Dequeue a task event
*/
dequeueEvent :: !*IWorld -> (!MaybeError TaskException (Maybe (InstanceNo,Event)),!*IWorld)

/**
* Remove all events for a given instance
*/
clearEvents :: !InstanceNo !*IWorld -> *IWorld

/**
* Queue ui change task output
*/
queueUIChange :: !InstanceNo !UIChange !*IWorld -> *IWorld
/**
* Convenience function that queues multiple changes at once
*/
queueUIChanges :: !InstanceNo ![UIChange] !*IWorld -> *IWorld
/**
* Queue exception change task output
*/
queueException :: !InstanceNo !String !*IWorld -> *IWorld
/**
* When a new viewport is attached to an instance, all events and output are removed
* and a single Reset event is queued
*/
attachViewport :: !InstanceNo !*IWorld -> *IWorld

/**
* When a new viewport is detached from an instance, all events and output are removed
*/
detachViewport :: !InstanceNo !*IWorld -> *IWorld

