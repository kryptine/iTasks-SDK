definition module Task

/**
* This module provides types for the definition of tasks & changes.
*/

import SystemTypes, HTTP, GenVisualize, iTaskClass, GenRecord
from TaskContext		import :: TaskContextTree
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
			
:: TaskNr			:== [Int]		// task nr i.j is administrated as [j,i]

:: TaskInitFun		:== TaskNr *IWorld -> *(!TaskContextTree,!*IWorld)
:: TaskEditFun		:== TaskNr EditEvent TaskContextTree *IWorld -> *(!TaskContextTree,!*IWorld)
:: TaskEvalFun a	:== TaskNr (Maybe CommitEvent) ReversedTaskNr TaskRepTarget TaskContextTree *IWorld -> *(!TaskResult a, !*IWorld)

:: TaskRepTarget
	= RepAsTUI (Maybe Layout)	//Optionally with tweaked layout
	| RepAsService
	
:: ReversedTaskNr	:== [Int]							//Reversed tasks nr used to locate a subtask in a composition  

:: Event e			= ProcessEvent	!ReversedTaskNr !e	//Event for a process we have not evaluated yet
					| TaskEvent		!ReversedTaskNr !e	//Event for a task within the process we are looking for
					| LuckyEvent	!e					//Event for any task who is willing to handle it (I am feeling lucky event)

:: EditEvent		:== Event (!String,!JSONNode)		//Datapath and new value
:: CommitEvent		:== Event String					//Action name


:: TaskResult a		= TaskInstable	!(Maybe a)	!TaskRep !TaskContextTree
					| TaskStable	!a			!TaskRep !TaskContextTree
					| TaskException	!Dynamic !String

:: TaskRep
	= NoRep
	| TUIRep !TaskTUI
	| ServiceRep !TaskServiceRep 

//Task representation for web application format
:: TaskTUI		:== (!Maybe TUIDef, ![TaskAction], ![TaskAttribute]) 

//Task representation for web service format
:: TaskServiceRep	:== (![TaskPart], ![TaskAction])

:: TaskPart			:== (!TaskId, !Int, !JSONNode) //Task id, part index, value
:: TaskAction		:== (!TaskId, !Action, !Bool)
:: TaskAttribute	:== (!String, !String) 

//Creates an execption result
taskException :: !e -> TaskResult a | TC, toString e

/**
* Create a task from a description and a pair of task functions
*
*/
mkTask :: !TaskInitFun !TaskEditFun !(TaskEvalFun a) -> Task a 

/**
* Create a task that is immediately finished
*/
mkInstantTask :: (TaskNr *IWorld -> (!TaskResult a,!*IWorld)) -> Task a | iTask a


class iTaskId a
where
	iTaskId :: !a !String 	-> String
	
instance iTaskId TaskNr
instance iTaskId TaskId

/**
* Parses a formatted task number to its integer list representation
*
* @param The task nr as formatted string
*
* @return The task nr as integer list
*/
taskNrFromString 	:: !String -> TaskNr
/**
* Converts a task number to its dotted string representation
*
* @param The task number as integer list
*
* @return The formatted task number
*/
taskNrToString		:: !TaskNr -> String

/**
* Helper function for directing possible commit events to the right location
* in a TaskContext. If there is an event and it is on the right path
* make a 'step' towards its destination by removing a segment from the path
*/
stepEvent :: !Int !(Maybe (Event e)) -> Maybe (Event e) 

stepTarget :: !Int !ReversedTaskNr -> ReversedTaskNr

// Changes

// A dynamic that contains a change
:: ChangeDyn	:== Dynamic

// A change function which may be used to change tasks at runtime
:: Change a :== (TaskInstanceMeta (Task a) (Task a) -> (Maybe TaskInstanceMeta, Maybe (Task a), Maybe ChangeDyn))

// Changes may be applied only once, or persist for future changes
:: ChangeLifeTime	= CLTransient
					| CLPersistent !ChangeLabel

//A label for identifying changes externally
:: ChangeLabel	:== String

//A labeled new change
:: ChangeInjection :== (!ChangeLifeTime,!ChangeDyn)
