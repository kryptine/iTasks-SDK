definition module CoreTasks
/**
* This module provides the core 'basic tasks' from which more specialized tasks can be derived.
*/

import iTaskClass
from Error				import ::MaybeError(..)
from OSError			import ::MaybeOSError, ::OSError, ::OSErrorCode, ::OSErrorMessage
from SharedCombinators	import :: ReadWriteShared, :: Shared
from Task				import :: Task, ::ChangeLifeTime, :: ChangeDyn, :: InteractionTerminators

:: SharedStoreId :== String

/**
* Lifts a value to the task domain. The task finishes immediately and yields its parameter
* as result of the task.
*
* @param Value: The value to be returned
* @return A task that will return the value defined by the parameter
* 
* @gin-icon return
*/
return 		:: !a 										-> Task a 		| iTask a

/*
* Creates a reference to a store identified by a string identifier.
* If no data is store the default value given as second argument is given as result.
*/
sharedStore :: !SharedStoreId !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

/**
* Reads shared data.
*
* @param ReadWriteShared: A shared reference
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
* @param ReadWriteShared: A shared reference
* @param Value: A value to write
* @return The value written
* @throws SharedException
*
* @gin-title Write shared
* @gin-icon shared_update
*/
set :: !(ReadWriteShared r a) !a -> Task a | iTask a

/**
* Updates shared data in one atomic operation.
*
* @param Update function: A function modifying the shared value
* @param ReadWriteShared: A shared reference
* @return The value written
* @throws SharedException
*
* @gin-title Update shared
* @gin-icon shared_update
*/
update :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w

/**
* Swiss-army-knife interaction tasks. All other interaction tasks are derived from this one.
*
* An interaction tasks works on a shared data model (r w). Additonally interation tasks keep a local state (l).
* How the data model is displayed/updated/changed is defined by means of dynamically calculated InteractionParts.
* When the tasks stop and it's result (a) is determined by dynamically calculated InteractionTerminators.
*
* @param Description: A description of the task to display to the user
* @param Interaction function: A function (on current local state, current shared state & flag indicating if shared state has changed since last edit event for this task)
*        dynamically generating the interaction parts shown to the user (parts can change the local state (l) & possibly also write to the shared (Maybe w));
*        Additionally the local state can be updated
* @param Initial state: The initial local state
* @param ReadWriteShared: A reference to shared data the task works on
*
* @return A result determined by the terminators
* @throws SharedException
*
* @gin False
*/
interact :: !d !(l r Bool -> [InteractionPart (!l,!Maybe w)]) l !(ReadWriteShared r w) -> Task (l,r) | descr d & iTask l & iTask r & iTask w

:: InteractionPart o	= E.v:	UpdateView	!(FormView v) !((Maybe v) -> o)	& iTask v	// A view on the data model (FormView v) which also allows update the states on change ((Maybe v) -> o) (the Maybe indicates if the form is produces a valid value)
						| E.v:	DisplayView	!v								& iTask v	// A static view displayed to the user
						|		Update		!String !o									// A interaction element (typically a button with a string-label) allowing to directly change the states
				
:: FormView v	= FormValue !v				// A form representing a value
				| Blank						// A blank form
				| Unchanged (FormView v)	// Form is unchanged, if no view is stored the given initial value is used

/**
* Dynamically adds a workflow to the system.
*
* @param Workflow: The workflow to add
* @return The description of the added workflow
* 
* @gin False
*/
addWorkflow :: !Workflow -> Task WorkflowDescription

/**
* Administer a change to another (running) workflow process
*
* @param Process ID: A process id
* @param Change dynamic: The change
* @param Lifetime: The change's lifetime
*
* @return The task that will do the change
* 
* @gin False
*/
applyChangeToProcess :: !ProcessId !ChangeDyn !ChangeLifeTime  -> Task Void

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
