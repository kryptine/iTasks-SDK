definition module Task

/**
* This module provides types for the definition of tasks & changes.
*/

import SystemTypes, HTTP, GenVisualize, iTaskClass, GenRecord
from TaskState			import :: TaskState
from LayoutCombinators	import :: Layout

derive JSONEncode		Task
derive JSONDecode		Task
derive gUpdate			Task
derive gDefaultMask		Task
derive gVerify			Task
derive gVisualizeText	Task
derive gVisualizeEditor	Task
derive gHeaders			Task
derive gGridRows		Task
derive gEq				Task
derive gGetRecordFields	Task
derive gPutRecordFields	Task

// Tasks
:: Task a =
	{ eval					:: !TaskEvalFun a
	, layout				:: !Maybe Layout	//Optional tweaked layout algorithm
	}
				
:: TaskInitFun		:== TaskState TaskId *IWorld -> *(!TaskState,!*IWorld)
:: TaskEvalFun a	:== (Maybe EditEvent) (Maybe CommitEvent) RefreshFlag TaskRepTarget TaskState *IWorld -> *(!TaskResult a, !*IWorld)

:: RefreshFlag		:== Bool

:: Event e			= TaskEvent		!TaskId !e			//Event for a task within the process we are looking for
					| LuckyEvent	!e					//Event for any task who is willing to handle it (I am feeling lucky event)

:: EditEvent		:== Event (!String,!JSONNode)		//Datapath and new value
:: CommitEvent		:== Event String					//Action name


:: TaskResult a		= ValueResult !(TaskValue a) !TaskTime !TaskRep !TaskState	//If all goes well, a task computes its current value, an observable representation and a new task state
					| ExceptionResult !Dynamic !String							//If something went wrong, a task produces an exception value

:: TaskRepTarget
	= RepAsTUI (Maybe TaskId) (Maybe Layout)	//Optionally with tweaked layout
	| RepAsService (Maybe TaskId)

:: TaskRep
	= NoRep
	| TUIRep !TaskTUI
	| ServiceRep !TaskServiceRep 

//Task representation for web application format
:: TaskTUI		:== (!TaskCompositionType, !Maybe TUIDef, ![TaskAction], ![TaskAttribute]) 

//Task representation for web service format
:: TaskServiceRep	:== (![TaskPart], ![TaskAction], ![TaskAttribute])

//Summary of the composition structure of tasks (used as input for layouting)
:: TaskCompositionType
	= ViewPart
	| SingleTask
	| SequentialComposition
	| ParallelComposition

:: TaskPart			:== (!String, !JSONNode)		//Task id, part index, value
:: TaskAction		:== (!String, !Action, !Bool)	//Task id, action, enabled
:: TaskAttribute	:== (!String, !String) 

/**
* Creates an execption result
*/
exception :: !e -> TaskResult a | TC, toString e
/**
* Create a task from a description and a pair of task functions
*
*/
mkTask :: !(TaskEvalFun a) -> Task a 
/**
* Create a task that finishes instantly
*/
mkInstantTask :: (TaskId *IWorld -> (!TaskResult a,!*IWorld)) -> Task a | iTask a

//* Provides fmap for Task Values
instance Functor TaskValue

