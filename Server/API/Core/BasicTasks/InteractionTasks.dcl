definition module InteractionTasks

import Maybe, iTaskClass, Task
from Shared		import :: Shared
from Types		import :: Action

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
* @return A result determined by the terminators
*/
interact		:: !d !(l r Bool -> [InteractionPart (!l,!Maybe w)])	!(l r Bool -> InteractionTerminators a)	!l !(Shared r w)	-> Task a | descr d & iTask l & iTask a & iTask w
/**
* A derived version of 'interact' which only uses a local state.
*/
interactLocal	:: !d !(l -> [InteractionPart l])						!(l -> InteractionTerminators a)		!l					-> Task a | descr d & iTask l & iTask a

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
:: Verified a	= Invalid
				| Valid !a

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