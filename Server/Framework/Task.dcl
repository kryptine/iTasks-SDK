definition module Task

/**
* This module provides types for the definition of tasks & changes.
*/

import Types, HTTP, GenVisualize, iTaskClass, GenRecord
from TaskContext	import :: TaskContext

derive JSONEncode		Task
derive JSONDecode		Task
derive gUpdate			Task
derive gDefaultMask		Task
derive gVerify			Task
derive gVisualize		Task
derive gEq				Task
derive gGetRecordFields	Task
derive gPutRecordFields	Task

// Tasks
:: Task a =
	{ properties			:: !TaskProperties						// the task's general properties
	, mbTaskNr				:: !(Maybe TaskNr)						// the task's identifier TODO: May be removed
	
	, type					:: !(TaskType a)
	}
	
:: TaskType a	= NormalTask !(TaskFuncs a)
				| ActionTask !(A.b: (TermFunc a b) -> TaskFuncs b | iTask b)
				
:: TaskFuncs a =	{ initFun				:: TaskInitFun
					, editEventFun			:: TaskEditEventFun
					, evalTaskFun			:: TaskEvalFun a
					}

:: TaskNr			:== [Int]		// task nr i.j is administrated as [j,i]

:: TaskInitFun		:== TaskNr *IWorld -> *(!TaskContext,!*IWorld)
:: TaskEditEventFun	:== TaskNr EditEvent TaskContext *IWorld -> *(!TaskContext,!*IWorld)
:: TaskEvalFun a	:== TaskNr (Maybe CommitEvent) ReversedTaskNr InteractionLayouter ParallelLayouter MainLayouter TaskContext *IWorld -> *(!TaskResult a, !*IWorld)

:: ReversedTaskNr	:== [Int]									//Reversed tasks nr used to locate a subtask in a composition  
:: EditEvent		:== (!ReversedTaskNr, !String, !JSONNode)	//Location, Datapath and new value
:: CommitEvent		:== (!ReversedTaskNr, !String)				//Location and action name

:: TaskResult a		= TaskBusy !(Maybe TUIDef) ![TaskAction] !TaskContext
					| TaskFinished !a
					| TaskException !Dynamic !String
					
:: InformationState s =	{ modelValue	:: !s		// the value of the data model the editor is working on
						, localValid	:: !Bool	// a flag indicating if the editor's local view is valid
						}
:: TermFunc a b :== (InformationState a) -> InteractionTerminators b

:: InteractionTerminators a	= UserActions		![(!Action,!Maybe a)]	// A list of actions the user can possibly trigger, actions with a Just-value stop the task with given result, others (Nothing) are disabled
							| StopInteraction	!a						// The task stops and produces result a

// Converts to task functions, ok action is added to action tasks
toTaskFuncs :: !(Task a) -> TaskFuncs a | iTask a

:: TaskThread a		=
	{ originalTask		:: Task a
	, currentTask		:: Task a
	}
	
:: TaskThreadParam a b	=
	{ originalTask		:: a -> Task b
	, currentTask		:: a -> Task b
	, title				:: !String
	}
	
taskException :: !e -> TaskResult a | TC, toString e

/**
* Create a task from a description and a pair of task functions
*
*/
mkTask :: !d !TaskInitFun !TaskEditEventFun !(TaskEvalFun a) -> Task a | descr d

/**
* Create a task that is immediately finished
*/
mkInstantTask :: !d (TaskNr *IWorld -> (!TaskResult a,!*IWorld)) -> Task a | descr d

/**
* Create a task which can be continued by different actions from another task.
*
*/
mkActionTask :: !d !(A.b: (TermFunc a b) -> TaskFuncs b | iTask b) -> Task  a | descr d

mapActionTask :: !((InformationState a) -> (InformationState b)) !(Task a) -> Task b

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
* in a TaskContext. If there is a commit event and it is on the right path
* make a 'step' towards its destination by removing a segment from the path
*/
stepCommitEvent :: !Int !(Maybe CommitEvent) -> Maybe CommitEvent 

stepTUITaskNr :: !Int !ReversedTaskNr -> ReversedTaskNr

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
