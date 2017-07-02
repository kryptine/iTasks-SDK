definition module iTasks.WF.Definition
/*
* This provides the types that define the core concepts for specifying workflow
*/

from iTasks._Framework.IWorld import :: IWorld
from iTasks._Framework.TaskState import :: TaskTree
from iTasks._Framework.TaskEval import :: TaskEvalOpts, :: TaskEvalInfo
from iTasks.UI.Definition import :: UIChange
from Text.JSON import :: JSONNode
from Data.Maybe import :: Maybe

from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks._Framework.Generic.Visualization import generic gText, :: TextFormat
from iTasks._Framework.Generic.Defaults import generic gDefault
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq

// Task definition:
:: Task a = Task !(Event TaskEvalOpts TaskTree *IWorld -> *(!TaskResult a, !*IWorld))

:: Event	= EditEvent		!TaskId !String !JSONNode 	//Update something in an interaction: Task id, edit name, value
			| ActionEvent	!TaskId !String				//Progress in a step combinator: Task id, action id
			| FocusEvent	!TaskId						//Update last event time without changing anything: Task id
			| RefreshEvent	!String 					//Nop event, just recalcalutate the entire task instance (the string is the reason for the refresh)
			| ResetEvent                                //Nop event, recalculate the entire task and reset output stream

:: TaskResult a
   //If all goes well, a task computes its current value, a ui effect and a new task state
   = ValueResult !(TaskValue a) !TaskEvalInfo !UIChange !TaskTree   
   //If something went wrong, a task produces an exception value
   | ExceptionResult !TaskException
   //If a task finalizes and cleaned up it gives this result
   | DestroyedResult

//* Task results
:: TaskValue a		= NoValue				
					| Value !a !Stability 
:: Stability		:== Bool

StableValue   a :== Value a True
UnstableValue a :== Value a False

:: TaskException    :== (!Dynamic,!String) //The dynamic contains the actual exception which can be matched, the string is an error message
/**
* Creates an exception
*/
exception :: !e -> TaskException | TC, toString e

// Task instantiation:

//* Each task instance can be identified by two numbers:
// - A unique number identifying the top-level state
// - A unique number the task within the the state
:: TaskId		= TaskId !InstanceNo !TaskNo
:: InstanceNo	:== Int
:: TaskNo		:== Int

//The iTask context restriction contains all generic functions that need to
//be available for a type to be used in tasks
class iTask a
	//Interaction
	| gEditor{|*|}
	//Visualization
	, gText{|*|}
	//Serialization
	, JSONEncode{|*|}
	, JSONDecode{|*|}
	//Data
	, gDefault{|*|}
	, gEq{|*|}
	, TC a

