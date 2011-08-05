definition module Task

/**
* This module provides types for the definition of tasks & changes.
*/

import SystemTypes, HTTP, GenVisualize, iTaskClass, GenRecord
from TaskContext	import :: TaskContextTree

derive JSONEncode		Task
derive JSONDecode		Task
derive gUpdate			Task
derive gDefaultMask		Task
derive gVerify			Task
derive gVisualizeText	Task
derive gVisualizeHtml	Task
derive gVisualizeEditor	Task
derive gEq				Task
derive gGetRecordFields	Task
derive gPutRecordFields	Task

// Tasks
:: Task a =
	{ properties			:: !TaskProperties						// the task's general properties	
	, type					:: !(TaskType a)
	}
	
:: TaskType a	= NormalTask !(TaskFuncs a)
				| ActionTask !(A.b: (TermFunc a b) -> TaskFuncs b | iTask b)
				
:: TaskFuncs a =	{ initFun			:: TaskInitFun
					, editFun			:: TaskEditFun
					, evalFun			:: TaskEvalFun a
					}

:: TaskNr			:== [Int]		// task nr i.j is administrated as [j,i]

:: TaskInitFun		:== TaskNr *IWorld -> *(!TaskContextTree,!*IWorld)
:: TaskEditFun		:== TaskNr EditEvent TaskContextTree *IWorld -> *(!TaskContextTree,!*IWorld)
:: TaskEvalFun a	:== TaskNr TaskProperties (Maybe CommitEvent) ReversedTaskNr InteractionLayouter ParallelLayouter MainLayouter TaskContextTree *IWorld -> *(!TaskResult a, !*IWorld)

:: ReversedTaskNr	:== [Int]							//Reversed tasks nr used to locate a subtask in a composition  

:: Event e			= ProcessEvent	!ReversedTaskNr !e	//Event for a process we have not evaluated yet
					| TaskEvent		!ReversedTaskNr !e	//Event for a task within the process we are looking for

:: EditEvent		:== Event (!String,!JSONNode)		//Datapath and new value
:: CommitEvent		:== Event String					//Action name


:: TaskResult a		= TaskBusy !(Maybe TUIDef) ![TaskAction] !TaskContextTree
					| TaskFinished !a
					| TaskException !Dynamic !String

:: TaskAction :== (TaskId,Action,Bool)

// Converts to task functions, ok action is added to action tasks
toTaskFuncs :: !(Task a) -> TaskFuncs a | iTask a

:: TaskThread a		=
	{ originalTask		:: Task a
	, currentTask		:: Task a
	}
	
taskException :: !e -> TaskResult a | TC, toString e

/**
* Create a task from a description and a pair of task functions
*
*/
mkTask :: !d !TaskInitFun !TaskEditFun !(TaskEvalFun a) -> Task a | descr d

/**
* Create a task that is immediately finished
*/
mkInstantTask :: !d (TaskNr *IWorld -> (!TaskResult a,!*IWorld)) -> Task a | descr d

/**
* Create a task which can be continued by different actions from another task.
*
*/
mkActionTask :: !d !(A.b: (TermFunc a b) -> TaskFuncs b | iTask b) -> Task  a | descr d

mapActionTask			:: !((InformationState a) -> (InformationState b))	!(Task a) -> Task b
mapActionTaskModelValue	:: !(a -> b)										!(Task a) -> Task b

/**
* Extracts the subject of a task
*
* @param The task
* @return The task's subject
*/
taskTitle			:: !(Task a)	-> String

/**
* Extracts the description of a task
*/
taskDescription		:: !(Task a)	-> String

/*
* Extracts the initial properties of a task
*
* @param The task
* @return The task's initial properties
*/
taskProperties		:: !(Task a)	-> TaskProperties

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
:: Change a :== (ProcessProperties (Task a) (Task a) -> (Maybe ProcessProperties, Maybe (Task a), Maybe ChangeDyn))

// Changes may be applied only once, or persist for future changes
:: ChangeLifeTime	= CLTransient
					| CLPersistent !ChangeLabel

//A label for identifying changes externally
:: ChangeLabel	:== String

//A labeled new change
:: ChangeInjection :== (!ChangeLifeTime,!ChangeDyn)
