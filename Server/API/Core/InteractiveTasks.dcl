definition module InteractiveTasks

// This module provides the underlying implementation of interactive tasks.

import Maybe, iTaskClass, Task
from Shared		import :: Shared
from Types		import :: Action
from StdFunc	import id, const
from TSt		import :: TaskFunctions, :: TaskFunctionEdit, :: TaskFunctionCommit

// A view mapping an input from a shared data source (i)
// to a view shown to the user (v)
// and finally data changed by the user back to the data source (o).
:: View				i v o	:== (!i -> v,!v i -> o)
// A view on a SymmetricShared, having the same read and write type.
:: SymmetricView	m v		:== View m v m
// The identity view
idView :== (id,const)

// This tuple is used to link actions to user interfaces.
// Its two parts represent the (what , when) aspects of actions.
// What: The conceptual action to be taken
// When: The condition that determine if the action can be taken
:: TaskAction a :== (!Action, !(Verified a) -> Bool)

//Wrapper for task values that indicates if value passes the verification step
:: Verified a	= Invalid
				| Valid !a
						
//Default predicates on editor values to use with actions
always		:: (Verified a) -> Bool
ifvalid		:: (Verified a) -> Bool
ifinvalid	:: (Verified a) -> Bool

:: About a	= 		AboutValue !a
			| E.o:	SharedAbout !(Shared a o)
			
:: InteractiveTaskMode i o	= EnterMode !(o -> i)				// enter new value, function o->i needed to make type system happy, should be id function
							| LocalUpdateMode !o !(o -> i)		// update local value, function o->i needed to make type system happy, should be id function
							| SharedUpdateMode !(Shared i o)	// update shared value

// macros for common cases (use id function for enter/local-update mode
Enter			:== EnterMode id
LocalUpdate v	:== LocalUpdateMode v id
SharedUpdate s	:== SharedUpdateMode s

// function possibly generating action event triggered automatically
:: AutoActionEvents a :== (Verified a) -> Maybe Action

makeInteractiveTask :: !(Maybe (About about)) !(about -> aboutV) !(View i v o) ![TaskAction i] !(Maybe (AutoActionEvents i)) !(InteractiveTaskMode i o) -> TaskFunctions (!Action, !Maybe i) | iTask i & iTask v & iTask o & iTask about & iTask aboutV

