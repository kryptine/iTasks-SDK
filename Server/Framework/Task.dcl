definition module Task

/**
* This module provides types for the definition of tasks & changes.
*/

import SystemTypes, HTTP, GenVisualize, iTaskClass, GenRecord
from TaskContext		import :: TaskState
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
	{ initFun				:: !TaskInitFun
	, editFun				:: !TaskEditFun
	, evalFun				:: !TaskEvalFun a
	, layout				:: !Maybe Layout	//Optional tweaked layout algorithm
	}
				
:: TaskInitFun		:== TaskId *IWorld -> *(!TaskState,!*IWorld)
:: TaskEditFun		:== EditEvent TaskState *IWorld -> *(!TaskState,!*IWorld)
:: TaskEvalFun a	:== (Maybe EditEvent) (Maybe CommitEvent) TaskRepTarget TaskState *IWorld -> *(!TaskResult a, !*IWorld)

:: Event e			= TaskEvent		!TaskId !e			//Event for a task within the process we are looking for
					| LuckyEvent	!e					//Event for any task who is willing to handle it (I am feeling lucky event)

:: EditEvent		:== Event (!String,!JSONNode)		//Datapath and new value
:: CommitEvent		:== Event String					//Action name


:: TaskResult a		= TaskUnstable	!(Maybe a)	!TaskRep !TaskState
					| TaskStable	!a			!TaskRep !TaskState
					| TaskException	!Dynamic !String

:: TaskRepTarget
	= RepAsTUI (Maybe TaskId) (Maybe Layout)	//Optionally with tweaked layout
	| RepAsService (Maybe TaskId)

:: TaskRep
	= NoRep
	| TUIRep !TaskTUI
	| ServiceRep !TaskServiceRep 

//Task representation for web application format
:: TaskTUI		:== (!Maybe TUIDef, ![TaskAction], ![TaskAttribute]) 

//Task representation for web service format
:: TaskServiceRep	:== (![TaskPart], ![TaskAction], ![TaskAttribute])

//Summary of the composition structure of tasks (used as input for layouting)
:: TaskCompositionType
	= SingleTask
	| SequentialComposition
	| ParallelComposition

:: TaskPart			:== (!String, !Int, !JSONNode)	//Task id, part index, value
:: TaskAction		:== (!String, !Action, !Bool)	//Task id, action, enabled
:: TaskAttribute	:== (!String, !String) 

//Creates an execption result
taskException :: !e -> TaskResult a | TC, toString e
/**
* Create a task from a description and a pair of task functions
*
*/
mkTask :: !TaskInitFun !TaskEditFun !(TaskEvalFun a) -> Task a 
/**
* Create a task that is finishes instantly
*/
mkInstantTask :: (TaskId *IWorld -> (!TaskResult a,!*IWorld)) -> Task a | iTask a
