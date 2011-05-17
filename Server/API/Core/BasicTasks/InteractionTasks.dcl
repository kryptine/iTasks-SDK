definition module InteractionTasks

import Maybe, iTaskClass, Task
from Shared		import :: Shared
from Types		import :: Action
from CoreTasks	import :: InteractionPart, :: InteractionTerminators
/**
* A derived version of 'interact' which only uses a local state.
*/
interactLocal	:: !d !(l -> [InteractionPart l])						!(l -> InteractionTerminators a)		!l					-> Task a | descr d & iTask l & iTask a

/*
* Ask the user to choose an action. The list of actions is calculated dynamically.
*
* @param description 				A description of the task to display to the user
* @param (r -> [(Action,Maybe a)]) 	A list of actions the user can choose from. Each actions yields the given result if it's chosen & result is present (Just). Otherwise (Nothing) action is disabled.
* @param (Shared r w)				The shared value to use. 
*
* @return 							Value associated with chosen action.
*/						
chooseAction 		:: !d !(r -> [(!Action,!Maybe a)]) !(Shared r w)	-> Task a | descr d & iTask a & iTask w

/*
* Ask the user to choose an action. 
*
* @param description 		A description of the task to display to the user
* @param [(Action,a)]		A list of actions the user can choose from. Each actions yields the given result if it's chosen. 
*
* @return 					Value associated with chosen action.
*/
chooseActionConst	:: !d ![(!Action,a)]								-> Task a | descr d & iTask a
