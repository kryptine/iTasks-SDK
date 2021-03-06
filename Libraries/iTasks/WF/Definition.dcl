definition module iTasks.WF.Definition
/*
* This provides the types that define the core concepts for specifying workflow
*/

from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.TaskEval import :: TaskEvalOpts, :: TaskEvalInfo
from iTasks.UI.Definition import :: UIChange
from Text.GenJSON import :: JSONNode
from Data.Maybe import :: Maybe
from Data.Map import :: Map(..)
from Data.Set import :: Set
from Data.Functor import class Functor
from System.Time import :: Timestamp, :: Timespec

from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from Text.GenJSON import generic JSONEncode, generic JSONDecode
from Data.GenEq import generic gEq
from StdString import class toString, class fromString
from StdClass import class <
from StdOverloaded import class ==

// Task definition:
:: Task a =: Task (Event TaskEvalOpts *IWorld -> *(TaskResult a, *IWorld))

:: Event
	= EditEvent    !TaskId !String !JSONNode //* Update something in an interaction: Task id, edit name, value
	| ActionEvent  !TaskId !String           //* Progress in a step combinator: Task id, action id
	| RefreshEvent !(Set TaskId)             //* Recalcalutate the tasks with given IDs, using the current SDS values
	| ResetEvent                             //* Nop event, recalculate the entire task and reset output stream
	| ReadEvent
	| DestroyEvent                           //* Cleanup and remove a task

:: TaskResult a
   //If all goes well, a task computes its current value, a ui effect and a new task state
   = ValueResult !(TaskValue a) !TaskEvalInfo !UIChange !(Task a)
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

instance Functor TaskValue
/**
* Creates an exception
*/
exception :: !e -> TaskException | TC, toString e

:: ExceptionList =: ExceptionList [TaskException]
instance toString ExceptionList

// Task instantiation:

//* Each task instance can be identified by two numbers:
// - A unique number identifying the top-level state
// - A unique number the task within the the state
:: TaskId		= TaskId !InstanceNo !TaskNo
:: InstanceNo	:== Int
:: TaskNo		:== Int

:: TaskAttributes :== Map String JSONNode
:: InstanceKey  :== String

instance toString	TaskId
instance fromString	TaskId
instance ==			TaskId
instance <			TaskId

class toInstanceNo t :: t -> InstanceNo
instance toInstanceNo InstanceNo
instance toInstanceNo TaskId

:: TaskList a :== (!TaskId,![TaskListItem a])

:: TaskListItem a =
	{ taskId            :: !TaskId
	, listId            :: !TaskId
	, detached          :: !Bool
	, self              :: !Bool
	, value             :: !TaskValue a
	, taskAttributes       :: !TaskAttributes
	, managementAttributes :: !TaskAttributes
	}

:: TaskListFilter =
	//Which rows to filter
	{ onlyIndex         :: !Maybe [Int]
	, onlyTaskId        :: !Maybe [TaskId]
	, notTaskId         :: !Maybe [TaskId]
	, onlyAttribute 	:: !Maybe (!String,!JSONNode)
	, onlySelf          :: !Bool
	//What to be notified for
	, includeValue                :: !Bool
	, includeTaskAttributes       :: !Bool
	, includeManagementAttributes :: !Bool
	, includeProgress             :: !Bool
	}


fullTaskListFilter :: TaskListFilter

//Session tasks have acess to persistent data stored in clients
:: Cookies :== Map String String

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
	, gEq{|*|}
	, TC a
