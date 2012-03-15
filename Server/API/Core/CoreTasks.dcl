definition module CoreTasks
/**
* This module provides the core 'basic tasks' from which more specialized tasks can be derived.
*/

import iTaskClass, Shared
from Error				import ::MaybeError(..)
from OSError			import ::MaybeOSError, ::OSError, ::OSErrorCode, ::OSErrorMessage
from Task				import :: Task

derive class iTask WorkOnProcessState

/**
* Lifts a value to the task domain. The task finishes immediately and yields its parameter
* as result of the task.
*
* @param Value: The value to be returned
*				@default Void
* @return A task that will return the value defined by the parameter
* 
* @gin-icon return
* @gin-shape return
*/
return 		:: !a 										-> Task a 		| iTask a

/**
* Exception throwing. This will throw an exception of arbitrary type e which has to be caught
* by a higher level exception handler combinator.
*
* @param Value: The exception value
* @return The combined task
* 
* @gin-title Raise exception
* @gin-icon error
*/
throw		:: !e 								-> Task a 	| iTask a & iTask, toString e

/**
* Reads shared data once.
*
* @param Shared: A shared reference
* @return The value read
* @throws SharedException
*
* @gin-title Read shared
* @gin-icon shared_read
*/
get :: !(ReadWriteShared a w) -> Task a | iTask a

/**
* Writes shared data.
*
* @param Value: A value to write
* @param Shared: A shared reference
* @return The value written
* @throws SharedException
*
* @gin-title Write shared
* @gin-icon shared_update
*/
set :: !a !(ReadWriteShared r a) -> Task a | iTask a

/**
* Updates shared data in one atomic operation.
*
* @param Shared: A shared reference
* @param Update function: A function modifying the shared value
* @return The value written
* @throws SharedException
*
* @gin-title Update shared
* @gin-icon shared_update
*/
update :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w

/**
* Reads shared data continously
*
* @param Shared: A shared reference
* @return The value read
* @throws SharedException
*
* @gin-title Read shared
* @gin-icon shared_read
*/
watch :: !(ReadWriteShared r w) -> Task r | iTask r

/**
* Swiss-army-knife interaction tasks. All other interaction tasks are derived from this one.
*
* An interaction tasks works on a shared data model (r w). Additonally interation tasks keep a local state (l).
* How the data model is displayed/updated/changed is defined by means of dynamically calculated InteractionParts.
*
* @param Description: A description of the task to display to the user
* @param Local initialization: Initialize the local state
* @param Parts: TODO
* @param Initial state: The optional initial local state
* @param ReadWriteShared: A reference to shared data the task works on
*
* @return A result determined by the terminators
* @throws SharedException
*
* @gin False
*/
interact :: !d ![InteractionPart l r] !l !(ReadOnlyShared r) -> Task (l,r) | descr d & iTask l & iTask r

:: InteractionPart l r	= E.v: FormPart (FormInitFun l r v) (FormShareUpdateFun l r v) (FormViewUpdateFun l r v) & iTask v

:: FormInitFun l r v		:==	l r -> FormView v									// Create the initial form
:: FormShareUpdateFun l r v	:== l r (Maybe v) FormDirty	-> (l, Maybe (FormView v))	// What to do when share changes
:: FormViewUpdateFun l r v	:== l r (Maybe v)			-> (l, Maybe (FormView v))	// What to do when the view changes

:: FormView v	= BlankForm						// A blank form
				| FilledForm !v					// A filled in form
:: FormDirty	:== Bool						// Has a form been touched by the user

/**
* State of another process the user works on.
*/
:: WorkOnProcessState
	= WOActive		//* the process is active, the user can work on it
	| WOSuspended	//* the process is suspended
	| WOFinished	//* the process is finished
	| WOExcepted	//* an uncaught exception was thrown inside of the process

/**
* Work on another process.
*
* @param Process ID: The ID of the process to work on
* 
* @return The state of the process to work on
* @throws WorkOnException
*/
workOn :: !TaskId -> Task WorkOnProcessState

/**
* Evaluate a "World" function that does not yield any result once.
*
* @param World function: The function to evaluate
* @return A Void task that evaluates the function
* 
* @gin False
*/
appWorld :: !(*World -> *World)			-> Task Void

/**
* Evaluate a "World" function that also returns a value once.
*
* @param World function: The function to evaluate
* @return A task that evaluates the function and yield a
* 
* @gin False
*/
accWorld :: !(*World -> *(!a,!*World))	-> Task a | iTask a

/**
* Evaluate a "World" function that also returns a MaybeError value.
* If the MaybeError value is Error, the error is transformed.
* @param World function: The function to evaluate
* @param Error function: Error transformation function
*
* @return A Void task that evaluates the function
* 
* @gin False
*/
accWorldError   :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err

/**
* Evaluate a "World" function that also returns a MaybeOSError value.
* If the MaybeError value is Error, the error is transformed.
* @param World function: The function to evaluate
* @param Error function: Error transformation function
*
* @return A Void task that evaluates the function
* 
* @gin False
*/
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World))             -> Task a | iTask a
