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
derive gVisualizeText	Task
derive gEditor			Task
derive gEditMeta		Task
derive gEq				Task

// Tasks
:: Task a = Task !(Event TaskRepOpts TaskTree *IWorld -> *(!TaskResult a, !*IWorld))

:: Event	= EditEvent		!EventNo !TaskId !String !JSONNode		//Update something in an interaction: Task id, edit name, value
			| ActionEvent	!EventNo !TaskId !String				//Progress in a step combinator: Task id, action id
			| FocusEvent	!EventNo !TaskId						//Update last event time without changing anything: Task id
			| RefreshEvent	!(Maybe EventNo)						//Nop event, just recalcalutate the entire task instance
            | ResetEvent                                            //Nop event, recalculate the entire task and reset output stream

:: EventNo	:== Int	

:: TaskResult a		= ValueResult !(TaskValue a) !TaskInfo !TaskRep !TaskTree							//If all goes well, a task computes its current value, an observable representation and a new task state
					| ExceptionResult !Dynamic !String													//If something went wrong, a task produces an exception value
					| DestroyedResult																	//If a task finalizes and cleaned up it gives this result

:: TaskInfo =
	{ lastEvent			:: !TaskTime		//When was the last edit, action or focus event in this task
	, refreshSensitive	:: !Bool			//Can refresh events change the value or ui of this task (e.g. because shared data is read)
	}

:: TaskRepOpts	=
	{ useLayout			:: Maybe LayoutRules
	, modLayout			:: Maybe (LayoutRules -> LayoutRules)
    , noUI              :: Bool
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
* 'downgrades' an event to a refresh, but keeps the client given event number
*/
toRefresh :: Event -> Event

/**
* Creates an execption result
*/
exception :: !e -> TaskResult a | TC, toString e

/**
* Determine the layout function for a rep target
*/
repLayoutRules :: !TaskRepOpts -> LayoutRules

/**
* Apply the final layout if necessary
*/
finalizeRep :: !TaskRepOpts !TaskRep -> TaskRep

/**
* Create a task that finishes instantly
*/
mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a
