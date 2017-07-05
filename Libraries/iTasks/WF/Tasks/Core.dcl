definition module iTasks.WF.Tasks.Core
/**
* This module provis the builtin basic tasks 
*/
import iTasks.WF.Definition
from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorCode, :: OSErrorMessage
from iTasks.UI.Editor import :: EditMode
from iTasks.UI.Prompt import class toPrompt
from iTasks.SDS.Definition import :: SDS, :: RWShared

/**
* Lifts a value to the task domain. The task finishes immediately and yields its parameter
* as result of the task.
*
* @param Value: The value to be returned
*				@default ()
* @return A task that will return the value defined by the parameter
* 
* @gin-icon return
* @gin-shape return
*/
treturn :: !a -> Task a | iTask a

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
throw :: !e -> Task a | iTask a & iTask, toString e

/**
* Evaluate a "World" function that does not yield any result once.
*
* @param World function: The function to evaluate
* @return A () task that evaluates the function
* 
* @gin False
*/
appWorld :: !(*World -> *World)			-> Task () //TODO (All of these versions should be one core)

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
* @return A  task that evaluates the function
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
* @return A task that evaluates the function
* 
* @gin False
*/
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World))             -> Task a | iTask a

:: OSException			= OSException !OSError
instance toString OSException

/**
* Core interaction task. All other interaction tasks are derived from this one.
*
* An interaction tasks works on a local state and has read-only access to shared data.
*
* @param Description: A description of the task to display to the user
* @param Edit mode: The type of interaction: viewing, entering or updating information
* @param ReadOnlyShared: A reference to shared data the task has access to
* @param Initialization function: Computes the initial local state and view
* @param Refresh function: Recomputes the local state and view when either the view is edited or the shared data changes.
* @param Custom editor: Optional custom editor for the interaction
*
* @return The local state
*
* @gin False
*/
interact :: !d !EditMode !(RWShared () r w)
				(r -> (l, v))                       //On init
				(v l v -> (l, v, Maybe (r -> w))) 	//On edit
				(r l v -> (l, v, Maybe (r -> w)))  	//On refresh
				(Maybe (Editor v)) -> Task (l,v) | toPrompt d & iTask l & iTask r & iTask v

