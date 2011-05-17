definition module CoreTasks
/**
* This module provides the core 'basic tasks' from which more specialized tasks can be derived.
*/

import iTaskClass
from Error		import ::MaybeError(..)
from OSError	import ::MaybeOSError, ::OSError, ::OSErrorCode, ::OSErrorMessage
from Shared		import :: SymmetricShared
from Task		import :: Task
from TSt		import ::ChangeLifeTime, :: ChangeDyn


/**
* Lifts a value to the task domain. The return_V task finishes immediately and yields its parameter
* as result of the task.
*
* @param The value to be returned
* @return A task that will return the value defined by the parameter
*/
return 		:: !a 										-> Task a 		| iTask a

:: SharedStoreId :== String

/**
* Creates a reference to a store identified by a string identifier.
* If no data is store the default value given as second argument is given as result.
*/
sharedStore :: !SharedStoreId !a -> SymmetricShared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

/**
* Create a shared store with automatically generated reference and given initial value.
* The store is automatically garbage collected after the process it was generated in terminates.
*
* @param An inital value
* @return A reference to the generated store
* @throws SharedException
*/
createSharedStore :: !a  -> Task (SymmetricShared a) | iTask a

/**
* Reads shared data.
*
* @param A shared reference
* @return The value read
* @throws SharedException
*/
get :: !(Shared a w) -> Task a | iTask a

/**
* Writes shared data.
*
* @param A value to write
* @param A shared reference
* @return The value written
* @throws SharedException
*/
set :: !(Shared r a) !a -> Task a | iTask a

/**
* Updates shared data in one atomic operation.
*
* @param A function modifying the shared value
* @param A shared reference
* @param The new value
* @throws SharedException
*/
update :: !(r -> w) !(Shared r w) -> Task w | iTask r & iTask w

/**
* Swiss-army-knife interaction tasks. All other interaction tasks are derived from this one.
*
* An interaction tasks works on a shared data model (r w). Additonally interation tasks keep a local state (l).
* How the data model is displayed/updated/changed is defined by means of dynamically calculated InteractionParts.
* When the tasks stop and it's result (a) is determined by dynamically calculated InteractionTerminators.
*
* @param A description of the task to display to the user
* @param A function (on current local state, current shared state & flag indicating if shared state has changed since last edit event for this task)
*        dynamically generating the interaction parts shown to the user (parts can change the local state (l) & possibly also write to the shared (Maybe w))
* @param A function (on current local state, current shared state & flag indicating if shared state has changed since last edit event for this task)
*        dynamically calculating the terminators of the task
* @param The initial local state
* @param A reference to shared data the task works on
*
* @return A result determined by the terminators
* @throws SharedException
*/
interact		:: !d !(l r Bool -> [InteractionPart (!l,!Maybe w)])	!(l r Bool -> InteractionTerminators a)	!l !(Shared r w)	-> Task a | descr d & iTask l & iTask a & iTask w

:: InteractionPart o	= E.v:	UpdateView	!(!FormView v, !(Maybe v) -> o)	& iTask v	// A view on the data model (FormView v) which also allows update the states on change ((Maybe v) -> o) (the Maybe indicates if the form is produces a valid value)
						| E.v:	DisplayView	!v								& iTask v	// A static view displayed to the user
						|		Update		!String !o									// A interaction element (typically a button with a string-label) allowing to directly change the states
				
:: FormView v	= FormValue !v				// A form representing a value
				| Blank						// A blank form
				| Unchanged (FormView v)	// Form is unchanged, if no view is stored the given initial value is used
				
:: InteractionTerminators a	= UserActions		![(!Action,!Maybe a)]	// A list of actions the user can possibly trigger, actions with a Just-value stop the task with given result, others (Nothing) are disabled
							| StopInteraction	!a						// The task stops and produces result a
							
							
// auxiliary types/function for derived interaction tasks

// This tuple is used to link actions to user interfaces.
// Its two parts represent the (what , when) aspects of actions.
// What: The conceptual action to be taken
// When: The condition that determine if the action can be taken
:: PredAction a :== (!Action, !a -> Bool)

//Wrapper for task values that indicates if value passes the verification step
:: Verified a	= Invalid | Valid !a

always		:: (Verified a) -> Bool
ifvalid		:: !(Verified a) -> Bool
ifinvalid	:: !(Verified a) -> Bool

:: Valid :== Bool

alwaysShared	:: (Valid,a) -> Bool
ifvalidShared	:: !(!Valid,a) -> Bool
ifinvalidShared	:: !(!Valid,a) -> Bool

mb2Ver :: !(Maybe a) -> Verified a
ver2Mb :: !(Verified a) -> Maybe a

okAction :: !(Maybe a) -> InteractionTerminators a
addAbout :: !(Maybe about) ![InteractionPart o] -> [InteractionPart o] | iTask about

fromPredActions			:: !(l r Bool -> p)	!(Action l r Bool -> a)	![PredAction p] -> (l r Bool -> InteractionTerminators a)
fromPredActionsLocal	:: !(l -> p)		!(Action l -> a)		![PredAction p] -> (l -> InteractionTerminators a)

/**
* Administer a change to another (running) workflow process
*
* @param A process id
* @param The change
* @param The change's lifetime
*
* @return The task that will do the change
*/
applyChangeToProcess :: !ProcessId !ChangeDyn !ChangeLifeTime  -> Task Void

/**
* Evaluate a "World" function that does not yield any result once.
*
* @param The function to evaluate
*
* @param A Void task that evaluates the function
*/
appWorld :: !(*World -> *World)			-> Task Void

/**
* Evaluate a "World" function that also returns a value once.
*
* @param The function to evaluate
*
* @param A Void task that evaluates the function
*/
accWorld :: !(*World -> *(!a,!*World))	-> Task a | iTask a

/**
* Evaluate a "World" function that also returns a MaybeError value.
* If the MaybeError value is Error, the error is transformed.
* @param The function to evaluate
* @param Error transformation function
*
* @param A Void task that evaluates the function
*/
accWorldError   :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err

accWorldOSError :: !(*World -> (!MaybeOSError a, !*World))             -> Task a | iTask a