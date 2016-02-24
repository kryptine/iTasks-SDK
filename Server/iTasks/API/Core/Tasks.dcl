definition module iTasks.API.Core.Tasks
/**
* This module provides the core 'basic tasks' from which more specialized tasks can be derived.
*/

import iTasks._Framework.Generic
import iTasks._Framework.SDS
from iTasks._Framework.Task			import :: Task, :: Event, :: ConnectionHandlers, :: TaskEvalOpts, :: TaskTime
from iTasks.UI.Definition 		import :: UI, :: UINodeType, :: UIChange
from iTasks.UI.Prompt 			import class toPrompt
from Data.Error					import ::MaybeError(..)
from System.OSError				import ::MaybeOSError, ::OSError, ::OSErrorCode, ::OSErrorMessage

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
treturn 		:: !a 										-> Task a 		| iTask a

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
upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w

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
* Core interaction task. All other interaction tasks are derived from this one.
*
* An interaction tasks works on a local state and has read-only access to shared data.
*
* @param Description: A description of the task to display to the user
* @param ReadOnlyShared: A reference to shared data the task has access to
* @param Initialization function: Computes the initial local state and view
* @param Refresh function: Recomputes the local state and view when either the view is edited or the shared data changes.
* @param Custom editor: Optional custom editor for the interaction
*
* @return The local state
*
* @gin False
*/
interact :: !d !(ReadOnlyShared r)
				(r -> (l,Masked v))
				(l r (Masked v) Bool Bool Bool -> (l,(Masked v)))
				(Maybe (Editor v)) -> Task l | toPrompt d & iTask l & iTask r & iTask v
/**
* Connect to an external system using TCP. This task's value becomes stable when the connection is closed
* @param Hostname
* @param Port
* @param A reference to shared data the task has access to
* @param The event handler functions
*/
tcpconnect :: !String !Int !(RWShared () r w) (ConnectionHandlers l r w) -> Task l | iTask l & iTask r & iTask w
/**
* Listen for connections from external systems using TCP.
* @param Port
* @param Remove closed connections. If this is true, closed connections are removed from the task value, if not they are kept in the list
* @param A reference to shared data the task has access to
* @param Initialization function: function that is called when a new connection is established
* @param Communication function: function that is called when data arrives, the connection is closed or the observed share changes.
*/
tcplisten :: !Int !Bool !(RWShared () r w) (ConnectionHandlers l r w) -> Task [l] | iTask l & iTask r & iTask w

/**
* Evaluate a "World" function that does not yield any result once.
*
* @param World function: The function to evaluate
* @return A () task that evaluates the function
* 
* @gin False
*/
appWorld :: !(*World -> *World)			-> Task ()

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

/**
* Write a value to the server console output for tracing
*/
traceValue :: a -> Task a | iTask a

/**
* Terminates a running task server
*/
shutDown :: Task ()

//INTERNAL FUNCTIONS EXPORTED FOR USE IN OPTIMIZED VERSIONS OF interact,
matchAndApplyEvent_ :: Event TaskId TaskEvalOpts (Maybe (Editor v)) TaskTime (Masked v) TaskTime d *IWorld
	-> *(!Masked v,!TaskTime,!*IWorld) | iTask v & toPrompt d
visualizeView_ :: TaskId TaskEvalOpts (Maybe (Editor v)) Event (Masked v) (Masked v) d *IWorld
	-> *(!UIChange,!Bool,!*IWorld) | iTask v & toPrompt  d
