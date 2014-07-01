definition module iTasks.Framework.Task
/**
* This module provides types for the definition of tasks.
*/

import iTasks.API.Core.Types
import iTasks.Framework.Generic

from iTasks.Framework.TaskState			import :: TaskTree
from iTasks.API.Core.LayoutCombinators	import :: LayoutRules
from Data.Map				import :: Map

derive JSONEncode		Task
derive JSONDecode		Task
derive gDefault			Task
derive gUpdate			Task
derive gVerify			Task
derive gText	        Task
derive gEditor			Task
derive gEditMeta		Task
derive gEq				Task

:: TaskDefInfo =
  { moduleName :: String
  , taskName   :: String
  }

// Tasks
:: Task a = Task (Maybe TaskDefInfo) !(Event TaskEvalOpts TaskTree *IWorld -> *(!TaskResult a, !*IWorld))

:: Event	= EditEvent		!EventNo !TaskId !String !JSONNode		//Update something in an interaction: Task id, edit name, value
			| ActionEvent	!EventNo !TaskId !String				//Progress in a step combinator: Task id, action id
			| FocusEvent	!EventNo !TaskId						//Update last event time without changing anything: Task id
			| RefreshEvent	!(Maybe EventNo)						//Nop event, just recalcalutate the entire task instance
            | ResetEvent                                            //Nop event, recalculate the entire task and reset output stream

:: EventNo	:== Int	

:: TaskResult a		= ValueResult !(TaskValue a) !TaskEvalInfo !TaskRep !TaskTree						//If all goes well, a task computes its current value, an observable representation and a new task state
					| ExceptionResult !TaskException													//If something went wrong, a task produces an exception value
					| DestroyedResult																	//If a task finalizes and cleaned up it gives this result
:: TaskException    :== (!Dynamic,!String) //The dynamic contains the actual exception which can be matched, the string is an error message

//Additional options to pass down the tree when evaluating a task
:: TaskEvalOpts	=
	{ useLayout			:: Maybe LayoutRules
	, modLayout			:: Maybe (LayoutRules -> LayoutRules)
    , noUI              :: Bool
    , callTrace         :: [Int] //References to tasks higher in evaluation who called a the current task
	}

//Additional information passed up from the tree when evaluating a task
:: TaskEvalInfo =
	{ lastEvent			:: !TaskTime	//When was the last edit, action or focus event in this task
    , involvedUsers     :: ![User]      //Which user identities are involved in the task
	, refreshSensitive	:: !Bool		//Can refresh events change the value or ui of this task (e.g. because shared data is read)
	}
	
:: TaskRep	= NoRep								//For some tasks no external representation is generated
			| TaskRep !UIDef !TaskServiceRep	//Compute both the UI and the raw service representation simultaneously
					
//Task representation for web service format
:: TaskServiceRep	:== [TaskPart]
:: TaskPart			:== (!String, !JSONNode)		//Task id, value

//Low level specific tasks that handle network connections
from Internet.HTTP import :: HTTPRequest
from iTasks.Framework.Engine import :: ConnectionType

:: ConnectionTask = ConnectionTask !(String *IWorld -> *(![String],!Bool,!Dynamic,!*IWorld)) !((Maybe String) Dynamic *IWorld -> *([String], !Bool, !Dynamic, !*IWorld)) !(Dynamic *IWorld -> *(!Dynamic, !*IWorld))
:: BackgroundTask = BackgroundTask !(*IWorld -> *IWorld)

/**
* Gets the task definition info from a task, if it exists
*/
reflect :: (Task a) -> Maybe TaskDefInfo

/**
* 'downgrades' an event to a refresh, but keeps the client given event number
*/
toRefresh :: Event -> Event

/**
* Creates an execption
*/
exception :: !e -> TaskException | TC, toString e

/**
* Determine the layout function for a rep target
*/
repLayoutRules :: !TaskEvalOpts -> LayoutRules

/**
* Apply the final layout if necessary
*/
finalizeRep :: !TaskEvalOpts !TaskRep -> TaskRep

/**
* Extend the call trace with the current task number
*/
extendCallTrace :: !TaskId !TaskEvalOpts -> TaskEvalOpts

/**
* Create a task that finishes instantly
*/
mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a
