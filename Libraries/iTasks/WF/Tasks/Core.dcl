definition module iTasks.WF.Tasks.Core
/**
* This module provis the builtin basic tasks
*/
import iTasks.WF.Definition
import iTasks.SDS.Definition

from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorCode, :: OSErrorMessage
from iTasks.UI.Editor import :: EditMode

/**
* Lifts a value to the task domain. The task finishes immediately and yields its parameter
* as result of the task.
*
* @param Value: The value to be returned
*				@default ()
* @return A task that will return the value defined by the parameter
*/
return :: !a -> Task a

//Backwards compatibility
treturn :== return

/**
* Exception throwing. This will throw an exception of arbitrary type e which has to be caught
* by a higher level exception handler combinator.
*
* @param Value: The exception value
* @return The combined task
*/
throw :: !e -> Task a | TC, toString e

/**
* Evaluate a "World" function that does not yield any result once.
*
* @param World function: The function to evaluate
* @return A () task that evaluates the function
*/
appWorld :: !(*World -> *World)			-> Task ()

/**
* Evaluate a "World" function that also returns a value once.
*
* @param World function: The function to evaluate
* @return A task that evaluates the function and yield a
*/
accWorld :: !(*World -> *(a,*World))	-> Task a

/**
* Evaluate a "World" function that also returns a MaybeError value.
* If the MaybeError value is Error, the error is transformed.
* @param World function: The function to evaluate
* @param Error function: Error transformation function
*
* @return A  task that evaluates the function
*/
accWorldError   :: !(*World -> (MaybeError e a, *World)) !(e -> err) -> Task a | TC, toString err

/**
* Evaluate a "World" function that also returns a MaybeOSError value.
* If the MaybeError value is Error, the error is transformed.
* @param World function: The function to evaluate
* @param Error function: Error transformation function
*
* @return A task that evaluates the function
*/
accWorldOSError :: !(*World -> (MaybeOSError a, *World))             -> Task a

:: OSException			= OSException !OSError
instance toString OSException

/**
 * Core interaction task. All other interaction tasks are derived from these ones.
 * `interactRW` is used for entering and updating data
 * `interactR` is almost identical but does not update the given sds and is used for viewing.
 */
interactRW :: (Editor r w) !(sds () (Maybe r) w) -> Task r | iTask r & TC r & TC w & RWShared sds

//* See documentation on `interactRW`.
interactR :: (Editor r w) (sds () (Maybe r) w) -> Task r | iTask r & TC r & TC w & Registrable sds

