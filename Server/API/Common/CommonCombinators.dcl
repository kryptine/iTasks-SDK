definition module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators.
*/
/* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators */

import CoreCombinators, TuningCombinators
import Either
from SystemTypes		import :: User, :: SessionId
from Map				import :: Map

// Additional types for grouping
// These types are similar to PAction but are needed to avoid circular definitions
//:: GAction		= GStop  | GContinue | GExtend [Task GAction] | GFocus Tag
//:: GOnlyAction	= GOStop | GOExtend [Task Void] | GOFocus Tag

//derive class iTask GAction, GOnlyAction
derive gVisualize	Tag
derive gUpdate		Tag
derive gDefaultMask	Tag
derive gVerify		Tag
derive JSONEncode	Tag
derive JSONDecode	Tag
derive gEq			Tag

/**
* General multi-bind used to define continuations.
* Similar to (>>+) but terminators yield a continuation which is executed directly.
*
* @param Task: The task for which continuations are defined
* @param Continuation terminator generator function: The functions generating the terminators yielding continuations
* @return The continuation's result
*
* @gin False
*/
(>>*) infixl 1 :: !(Task a) !(TermFunc a (Task b)) -> Task b | iTask a & iTask b


/**
* Special multi-bind used to define continuations using a list of user action with constant length.
* A list of continuation with conditions (always, ifvalid, sometimes) has to be provided.
*
* @param Task: The task for which continuations are defined
* @param Continuations: A list of continuations
*
* @return The continuation's result
*
* @gin False
*/
(>?*) infixl 1 :: !(Task a) ![(!Action,!TaskContinuation a b)] -> Task b | iTask a & iTask b

/**
* Adds a trigger to a task. The task automatically terminates as soon as the predicate holds.
*
* @param Task: The task to which the trigger is added
* @param Predicate: A predicate on the task's state
*
* @return The task's result
*
* @gin False
*/
(>?) infixl 1 :: !(Task a) !(a -> Bool) -> Task a | iTask a

/** 
* Continuation, used as second argument of multi-bind.
*/
:: TaskContinuation a b	= Always	!(Task b)									//* continuation which can always be taken
						| IfValid	!(a -> Task b)								//* continuation which can be taken if the local editor is in a valid state, the current value is given as input
						| Sometimes	!((InformationState a) -> Maybe (Task b))	//* continuation which can sometimes be taken depending on the editor's current state

/**
* Transform a value with a custom function
*
* @param Transformation function: The transformation function
* @param Value: The value to be transformed
*
* @return The transformed value
*
* @gin False
*/
transform			:: !(a -> b) !a 									-> Task b | iTask b

/**
* Assign a task to a(nother) user.
*
* @param Manager properties: The initial manager properties indicating the user to which the task is delegated, a priority and possibly a deadline
* @param Action menu: A function generating a menu for the process delegated to the user
* @param Task: The task that is to be delegated
*
* @return The combined task
*
* @gin False
*/ 
assign :: !ManagerProperties !(Task a) -> Task a | iTask a

/**
* Assign a task to a user. (no deadline, normal priority, no menu)
*
* @param User: The initial UserId of the user to which the task is delegated
* @param Task: The task that is to be delegated.
*
* @return The combined task
*
* @gin-title Assign to user
* @gin-icon user
* @gin-shape assign
*/
(@:) infix 3		:: !User !(Task a) -> Task a | iTask a

/**
* Combines two tasks sequentially just as >>=, but the result of the second task is disregarded.
*
* @param First: The first task to be executed
* @param Second: The second task to be executed
* 
* @return The combined task
*
* @gin False
*/
(>>^) infixl 1 :: !(Task a) (Task b) -> Task a| iTask a & iTask b

/**
* Bind for tasks with optional results.
*
* @param First:  The first task to be executed.
* @param Second: The function of the second task to be executed. It is only executed if the first produces a result.
*
* @return The result of the second task, or Nothing if the first task did not produce a result.
* 
* @gin False
*/
(>>?)	infixl 1	:: !(Task (Maybe a)) !(a -> Task (Maybe b))	-> Task (Maybe b) 		| iTask a & iTask b

/**
* Execute a Maybe task that you expect to always return Just.
* Throw an exception if it returns nothing
*
* @param The task that could in theory return Nothing
* @return The result of the task
* 
* @gin False
*/
justdo	:: !(Task (Maybe a)) -> Task a | iTask a

/**
* Execute the list of tasks one after another.
* 
* @param Label: A label for tracing
* @param Tasks: The list of tasks to be executed sequentially
* @return The combined task
* 
* @gin-icon sequence
*/
sequence	:: !String ![Task a] 						-> Task [a]		| iTask a

/**
* Repeats a task until a given predicate holds. The predicate is tested as soon as the
* given task is finished. When it does not hold, the task is restarted.
*
* @param Task: The task to be looped
* @param Predicate: The predicate over the result of the task to determine if the combination is finished
* @return The combined task
* 
* @gin False
*/
(<!)  infixl 6 	:: !(Task a)  !(a -> .Bool) 			-> Task a 		| iTask a

/**
* Repeats a task infinitely. As soon as the task is finished, it is restarted immediately.
* As a consequence, the combined task never finishes.
*
* @param Task: The task that has to be repeated infinitely
* @return The combined task
* 
* @gin False
*/
forever :: !(Task a) -> Task b | iTask a & iTask b

/**
* Group two tasks in parallel, return the result of the first completed task.
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the task that is completed first
* 
* @gin False
*/
(-||-) infixr 3 	:: !(Task a) !(Task a) 	-> Task a 				| iTask a

/**
* Group two tasks in parallel, return the result of the left task
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the task that is completed first
* 
* @gin False
*/
(||-)  infixr 3		:: !(Task a) !(Task b)	-> Task b				| iTask a & iTask b

/**
* Group two tasks in parallel, return the result of the right task
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the task that is completed first
* 
* @gin False
*/
(-||)  infixl 3		:: !(Task a) !(Task b)	-> Task a				| iTask a & iTask b

/** 
* Group two tasks in parallel that both need to be completed.
*
* @param Left: The left task
* @param Right: The right task
*
* @return The results of both tasks 
* 
* @gin-parallel True
* @gin-title Parallel merge (tuple)
* @gin-icon parallel-merge-tuple
*/
(-&&-) infixr 4 	:: !(Task a) !(Task b) 	-> Task (a,b) 			| iTask a & iTask b

/**
* Group two tasks in parallel that both need to be completed but
* can complete without a result.
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of both tasks if both finish with a result. Nothing otherwise.
* 
* @gin False
*/
(-&?&-)	infixr 4	:: !(Task (Maybe a)) !(Task (Maybe b)) 		-> Task (Maybe (a,b)) 	| iTask a & iTask b

/**
* Group a list of tasks in parallel.
* The group stops as soon as one result is available which is returned.
*
* @param Tasks: The list of tasks
*
* @return The first result
* 
* @gin-parallel True
* @gin-title Take first completed
* @gin-icon parallel-merge-first
*/
anyTask				:: ![Task a]			-> Task a				| iTask a

/**
* Group a list of tasks in parallel.
* The group stops when all tasks are completed.
*
* @param Tasks: The list of tasks
*
* @return The list of results
* 
* @gin-parallel True
* @gin-title Parallel merge (list)
* @gin-icon parallel-merge-list
*/
allTasks			:: ![Task a]			-> Task [a]				| iTask a

/**
* Group two tasks in parallel of which only one needs to be completed.
* The tasks can have different types. The 'Either' results indicates which task completed.
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the first completed task wrapped in an 'Either'.
* 
* @gin False
*/
eitherTask			:: !(Task a) !(Task b) 	-> Task (Either a b)	| iTask a & iTask b	

/*
/**
* Execute two tasks as separate main tasks.
* The composition is done as soon as one result is finished.
*
* @param The left task
* @param The right task
*
* @return The result of the first completed task.
*/
orProc 				:: !(ProcTask a) !(ProcTask a) -> Task a 	 	| iTask a
/**
* Execute two tasks as separate main tasks.
* The composition is done when both tasks are finished.
*
* @param The left task
* @param The right task
*
* @return The results of both tasks
*/
andProc 			:: !(ProcTask a) !(ProcTask b) -> Task (a,b) 	| iTask a & iTask b
/**
* Execute a list of tasks as separate main tasks.
* The composition is done as soon as one result is finished.
*
* @param The list of tasks
*
* @return The result of the first completed task.
*/
anyProc 			:: ![ProcTask a] 		   -> Task a 	 	| iTask a
/**
* Execute a list of tasks as separate main tasks.
* The composition is done when all tasks are finished.
*
* @param The list of tasks
*
* @return The list of results
*/
allProc 			:: ![ProcTask a] 		   -> Task [a] 	| iTask a

:: ProcTask a :== (!Task a,!ManagerProperties,!ActionMenu)
*/

/**
* Randomly selects one item from a list.
*
* @param Options: The list of options
*
* @return The chosen item
*/
randomChoice		:: ![a]										-> Task a				| iTask a

/**
* Iterate a task as long as a predicate is not valid.
*
* @param Task function: A task function to repeat. At each iteration the result of the previous iteration is given.
* @param Predicate: A predicate to test if we can stop.
* @param Initial value: An initial value for the first iteration.
*
* @return The result of the last iteration (that thus satisfies the predicate)
* 
* @gin False
*/
repeatTask		:: !(a -> Task a) !(a -> Bool) a 			-> Task a					| iTask a
/**
* Repeat a task as long as a predicate is not valid.
* If the predicate fails after an iteration an error message is given.
*
* @param Task: The task to repeat
* @param Predicate: The predicate/feedback function. This function also supplies the feedback message if the predicate yields False.
* 
* @return The result of the last iteration (that thus satisfies the predicate)
* 
* @gin False
*/
(<|)  infixl 6 	:: !(Task a)  !(a -> (Bool, [HtmlTag])) 	-> Task a 					| iTask a