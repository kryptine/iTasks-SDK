definition module Task

/**
* This module provides types for the definition of tasks & changes.
*/

import Types, HTTP, GenVisualize, iTaskClass, GenRecord
from TSt 		import :: TSt

derive JSONEncode		Task, TaskResult
derive JSONDecode		Task, TaskResult
derive gUpdate			Task, ManagerProperties, TaskPriority, RunningTaskStatus
derive gDefaultMask		Task, ManagerProperties, TaskPriority, RunningTaskStatus
derive gVerify			Task, ManagerProperties, TaskPriority, RunningTaskStatus
derive gVisualize		Task, ManagerProperties, TaskPriority, RunningTaskStatus
derive gEq				Task
derive gGetRecordFields	Task
derive gPutRecordFields	Task

// Tasks

:: Task a =
	{ properties			:: !TaskProperties						// the task's general properties
	, mbTaskNr				:: !(Maybe TaskNr)						// the task's identifier
	, taskFuncEdit			:: !(*TSt -> *TSt)						// a function on TSt implementing the task (process edit events pass)
	, taskFuncCommit		:: !(*TSt -> *(!TaskResult a,!*TSt))	// a function on TSt implementing the task (process commit events pass)
	, mbInteractionLayout	:: !Maybe InteractionLayoutMerger		// if present changes the layout of interaction tasks for this tasks and it's children
	, mbParallelLayout		:: !Maybe ParallelLayoutMerger			// if present changes the layout of parallel tasks for this tasks and it's children
	, mbResultLayout		:: !Maybe ResultLayoutMerger			// if present changes the layout of result panels for this tasks and it's children
	}

:: TaskNr			:== [Int]		// task nr i.j is administrated as [j,i]

:: TaskResult a		= TaskBusy
					| TaskFinished !a
					| TaskException !Dynamic !String
					
taskException :: !e -> TaskResult a | TC, toString e
					
mapTaskResult				:: !(a -> b) !(TaskResult a)				-> TaskResult b
mapTask						:: !(a -> b) !(Task a)						-> Task b

:: TaskThread a		=
	{ originalTask		:: !Task a
	, currentTask		:: !Task a
	}
	
:: TaskThreadParam a b	=
	{ originalTask		:: !a -> Task b
	, currentTask		:: !a -> Task b
	, title				:: !String
	}

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

incTaskNr :: !TaskNr -> TaskNr

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
