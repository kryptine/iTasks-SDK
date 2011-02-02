definition module Task

/**
* This module provides types for the definition of tasks & changes.
*/

import Types, Http, GenVisualize, iTaskClass
from TSt import :: TSt

derive JSONEncode	Task, TaskResult
derive JSONDecode	Task, TaskResult
derive gUpdate		Task
derive gDefaultMask	Task
derive gVerify		Task
derive gVisualize	Task
derive gEq			Task

// Tasks

:: Task a =
	{ taskProperties	:: !ManagerProperties					// the task's manager properties
	, groupedProperties	:: !GroupedProperties					// properties about how the tasks behaves inside of a group
	, mbTaskNr			:: !(Maybe TaskNr)						// the task's identifier
	, mbMenuGenFunc		:: !(Maybe MenuGenFunc)					// a function generating a menu structure
	, taskFunc			:: !(*TSt -> *(!TaskResult a,!*TSt))	// a function on TSt implementing the task
	}

:: TaskNr			:== [Int]		// task nr i.j is administrated as [j,i]

:: TaskResult a		= TaskBusy
					| TaskFinished !a
					| TaskException !Dynamic

:: TaskThread a		=
	{ originalTask		:: !Task a
	, currentTask		:: !Task a
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

/**
* Extracts the initial worker of a task
*
* @param The task
* @param The task's initial worker
*/
taskUser			:: !(Task a)	-> User
/*
* Extracts the initial properties of a task
*
* @param The task
* @return The task's initial properties
*/
taskProperties		:: !(Task a)	-> ManagerProperties

:: MenuId			:== Int

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
				
:: TaskEvent :== (!TaskId,!String,!JSONNode)	// taskid, name, value

// Changes

// A dynamic that contains a change
:: ChangeDyn	:== Dynamic

// A change function which may be used to change tasks at runtime
:: Change a :== (TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn))

// Changes may be applied only once, or persist for future changes
:: ChangeLifeTime	= CLTransient
					| CLPersistent !ChangeLabel

//A label for identifying changes externally
:: ChangeLabel	:== String

//A labeled new change
:: ChangeInjection :== (!ChangeLifeTime,!ChangeDyn)
