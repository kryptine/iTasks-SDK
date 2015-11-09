definition module iTasks._Framework.Task
/**
* This module provides types for the definition of tasks.
*/

import iTasks.API.Core.Types
import iTasks._Framework.Generic
from iTasks._Framework.Tonic.AbsSyn import :: ExprId (..)

from iTasks._Framework.TaskState			import :: TaskTree
from iTasks.UI.Diff     import :: UIChangeDef, :: UIStep
from Data.Map			import :: Map
from Data.CircularStack import :: CircularStack

derive JSONEncode		Task
derive JSONDecode		Task
derive gDefault			Task
derive gVerify			Task
derive gText	        Task
derive gEditor			Task
derive gEditMeta		Task
derive gEq				Task

// Tasks
:: Task a = Task !(Event TaskEvalOpts TaskTree *IWorld -> *(!TaskResult a, !*IWorld))

:: Event	= EditEvent		!TaskId !String !JSONNode 	//Update something in an interaction: Task id, edit name, value
			| ActionEvent	!TaskId !String				//Progress in a step combinator: Task id, action id
			| FocusEvent	!TaskId						//Update last event time without changing anything: Task id
			| RefreshEvent	!String 					//Nop event, just recalcalutate the entire task instance (the string is the reason for the refresh)
            | ResetEvent                                //Nop event, recalculate the entire task and reset output stream

:: TaskResult a		= ValueResult !(TaskValue a) !TaskEvalInfo !TaskRep !TaskTree						//If all goes well, a task computes its current value, an observable representation and a new task state
					| ExceptionResult !TaskException													//If something went wrong, a task produces an exception value
					| DestroyedResult																	//If a task finalizes and cleaned up it gives this result
:: TaskException    :== (!Dynamic,!String) //The dynamic contains the actual exception which can be matched, the string is an error message

//Additional options to pass down the tree when evaluating a task
:: TaskEvalOpts	=
	{ autoLayout 		:: Bool
    , noUI              :: Bool
    , tonicOpts         :: TonicOpts
	}

:: TonicOpts =
  { inAssignNode            :: Maybe ExprId
  , inParallel              :: Maybe TaskId
  , captureParallel         :: Bool
  , currBlueprintModuleName :: String
  , currBlueprintFuncName   :: String
  , currBlueprintTaskId     :: TaskId
  , currBlueprintExprId     :: ExprId
  , callTrace               :: CircularStack TaskId
  }

mkEvalOpts :: TaskEvalOpts
defaultTonicOpts :: TonicOpts

//Additional information passed up from the tree when evaluating a task
:: TaskEvalInfo =
	{ lastEvent			:: !TaskTime	        //When was the last edit, action or focus event in this task
    , removedTasks      :: ![(TaskId,TaskId)]   //Which embedded parallel tasks were removed (listId,taskId)
	, refreshSensitive	:: !Bool		        //Can refresh events change the value or ui of this task (e.g. because shared data is read)
	}
	
:: TaskRep	= NoRep							//For some tasks no external representation is generated
			| TaskRep !UIDef !UIChangeDef	//Compute both the UI and the changes simultaniously

//Low-level tasks that handle network connections
:: ConnectionTask = ConnectionTask !(ConnectionHandlersIWorld Dynamic Dynamic Dynamic) !(RWShared () Dynamic Dynamic)

//Definition of low-level network interaction

:: ConnectionHandlers l r w = 
    { onConnect         :: !(String r           -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , whileConnected    :: !((Maybe String) l r -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onDisconnect      :: !(               l r -> (!MaybeErrorString l, Maybe w                  ))
	}

//Version of connection handlers with IWorld side-effects that is still necessary for built-in framework handlers
:: ConnectionHandlersIWorld l r w =
    { onConnect         :: !(String r           *IWorld -> *(!MaybeErrorString l, Maybe w, ![String], !Bool, !*IWorld))
    , whileConnected    :: !((Maybe String) l r *IWorld -> *(!MaybeErrorString l, Maybe w, ![String], !Bool, !*IWorld))
    , onDisconnect      :: !(               l r *IWorld -> *(!MaybeErrorString l, Maybe w,                   !*IWorld))
    }

//Background computation tasks
:: BackgroundTask = BackgroundTask !(*IWorld -> *IWorld)

/**
* 'downgrades' an event to a refresh, but keeps the client given event number
*/
toRefresh :: Event -> Event

/**
* Creates an execption
*/
exception :: !e -> TaskException | TC, toString e

/**
* Extend the call trace with the current task number
*/
extendCallTrace :: !TaskId !TaskEvalOpts -> TaskEvalOpts

/**
* Wraps a set of connection handlers and a shared source as a connection task
*/
wrapConnectionTask :: (ConnectionHandlers l r w) (RWShared () r w) -> ConnectionTask | TC l & TC r & TC w
wrapIWorldConnectionTask :: (ConnectionHandlersIWorld l r w) (RWShared () r w) -> ConnectionTask | TC l & TC r & TC w

/**
* Create a task that finishes instantly
*/
mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a

