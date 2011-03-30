definition module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/

import CoreCombinators, TuningCombinators
import Either
from Types	import :: User, :: SessionId
from Map	import :: Map
from Shared	import :: SymmetricShared

// Additional types for grouping
// These types are similar to PAction but are needed to avoid circular definitions
:: GAction		= GStop  | GContinue | GExtend [Task GAction] | GFocus Tag
:: GOnlyAction	= GOStop | GOExtend [Task Void] | GOFocus Tag

derive class iTask GAction, GOnlyAction
derive gVisualize	Tag
derive gUpdate		Tag
derive gDefaultMask	Tag
derive gVerify		Tag
derive JSONEncode	Tag
derive JSONDecode	Tag
derive gEq			Tag

/**
* Transform a value with a custom function
*
* @param The transformation function
* @param The value to be transformed
*
* @return The transformed value
*/
transform			:: !(a -> b) !a 									-> Task b | iTask b
/**
* Assign a task to a(nother) user.
*
* @param The initial manger properties indicating the user to which the task is delegated, a priority and possibly a deadline
* @param A function generating a menu for the process delegated to the user
* @param The task that is to be delegated.
*
* @return The combined task
*/ 
assign :: !ManagerProperties !ActionMenu !(Task a) -> Task a | iTask a
/**
* Assign a task to a user. (no deadline, normal priority, no menu)
*
* @param The initial UserId of the user to which the task is delegated
* @param The task that is to be delegated.
*
* @return The combined task
*/
(@:) infix 3		:: !User !(Task a) -> Task a | iTask a
/**
* Combines two tasks sequentially just as >>=, but the result of the second task is disregarded.
*
* @param The first task to be executed
* @param The second task to be executed
* @return The combined task
*/
(>>^) infixl 1 :: !(Task a) (Task b) -> Task a| iTask a & iTask b
/**
* Bind for tasks with optional results.
*
* @param The first task to be executed.
* @param The function of the second task to be executed. It is only executed if the first produces a result.
*
* @return The result of the second task, or Nothing if the first task did not produce a result.
*/
(>>?)	infixl 1	:: !(Task (Maybe a)) !(a -> Task (Maybe b))	-> Task (Maybe b) 		| iTask a & iTask b
/**
* Execute a Maybe task that you expect to always return Just.
* Throw an exception if it returns nothing
*
* @param The task that could in theory return Nothing
* @return The result of the task
*/
justdo	:: !(Task (Maybe a)) -> Task a | iTask a

/**
* Repeats a task infinitely. As soon as the task is finished, it is restarted immediately.
* As a consequence, the combined task never finishes.
*
* @param The task that has to be repeated infinitely
* @return The combined task
*/
forever	t	:==	(<!) t (\_ -> False)

/**
* Group two tasks in parallel of which only one needs to be completed.
* -||-	: Done when one of the two tasks is done. It's result is returned.
*  ||-	: Done when the right task is done.
* -||	: Done when the left task is done.
*
* @param The left task
* @param The right task
*
* @return The result of the task that is completed first
*/
(-||-) infixr 3 	:: !(Task a) !(Task a) 	-> Task a 				| iTask a
(||-)  infixr 3		:: !(Task a) !(Task b)	-> Task b				| iTask a & iTask b
(-||)  infixl 3		:: !(Task a) !(Task b)	-> Task a				| iTask a & iTask b
/** 
* Group two tasks in parallel that both need to be completed.
*
* @param The left task
* @param The right task
*
* @return The results of both tasks 
*/
(-&&-) infixr 4 	:: !(Task a) !(Task b) 	-> Task (a,b) 			| iTask a & iTask b
/**
* Group two tasks in parallel that both need to be completed but
* can complete without a result.
*
* @param The left task
* @param The right task
*
* @param The result of both tasks if both finish with a result. Nothing otherwise.
*/
(-&?&-)	infixr 4	:: !(Task (Maybe a)) !(Task (Maybe b)) 		-> Task (Maybe (a,b)) 	| iTask a & iTask b

// old-style parallel, an overview table of all detached processes is shown in the parallel panel
oldParallel :: !d !pState !(ResultFun pState pResult) ![TaskContainer taskResult pState] -> Task pResult | iTask taskResult & iTask pState & iTask pResult & descr d

/**
* Group a list of tasks in parallel.
* The group stops as soon as one result is available which is returned.
*
* @param The list of tasks
*
* @return The first result
*/
anyTask				:: ![Task a]			-> Task a				| iTask a
/**
* Group a list of tasks in parallel.
* The group stops when all tasks are completed.
*
* @param The list of tasks
*
* @return The list of results
*/
allTasks			:: ![Task a]			-> Task [a]				| iTask a
/**
* Group two tasks in parallel of which only one needs to be completed.
* The tasks can have different types. The 'Either' results indicates which task completed.
*
* @param The left task
* @param The right task
*
* @param The result of the first completed task wrapped in an 'Either'.
*/
eitherTask			:: !(Task a) !(Task b) 	-> Task (Either a b)	| iTask a & iTask b	
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

/**
* Just returns Void. Used as a last step in tasks of type Void in combination with the >>| combinator.
*
* @return Void
*/
stop				:: Task Void
/**
* Randomly selects one item from a list.
*
* @param The list of options
*
* @return The chosen item
*/
randomChoice		:: ![a]										-> Task a				| iTask a
/**
* Iterate a task as long as a predicate is not valid.
*
* @param A task function to repeat. At each iteration the result of the previous iteration is given.
* @param A predicate to test if we can stop.
* @param An initial value for the first iteration.
*
* @param The result of the last iteration (that thus satisfies the predicate)
*/
repeatTask		:: !(a -> Task a) !(a -> Bool) a 			-> Task a					| iTask a
/**
* Repeat a task as long as a predicate is not valid.
* If the predicate fails after an iteration an error message is given.
*
* @param The task to repeat
* @param The predicate/feedback function. This function also supplies the feedback message if the predicate yields False.
* 
* @param The result of the last iteration (that thus satisfies the predicate)
*/
(<|)  infixl 6 	:: !(Task a)  !(a -> (Bool, [HtmlTag])) 	-> Task a 					| iTask a
/**
* Tasks can dynamically add other tasks or stop execution of group.
*
* @param List of initial tasks
*/
/*dynamicGroup		:: ![Task GAction]															-> Task Void
/**
* Tasks and group-actions can dynamically add other tasks or stop execution of group.
*
* @param List of initial tasks
* @param List of group-actions
*/
dynamicGroupA		:: ![Task GAction]	![GroupAction Void] !(GroupActionGenFunc GAction)		-> Task Void
/**
* Only group-actions can dynamically add other tasks or stop execution of group.
*
* @param List of initial tasks
* @param List of group-actions
*/
dynamicGroupAOnly	:: ![Task Void]		![GroupAction Void] !(GroupActionGenFunc GOnlyAction)	-> Task Void
/**
* Combinator for creating Multiple Document Interface (MDI) applications.
*
* @param An initial state for global application data
* @param A list of global application group actions
* @param A function generating a global application group action generation function.
*        The first parameter is a reference to the global state store.
*        The second parameter is a collection of tasks for dealing with editors.
* @param A global menu generation function, mapping the global state to a menu structure.
*/
mdiApplication ::
	!globalState
	![GroupAction Void]
	!((SymmetricShared (globalState,EditorCollection editorState)) (MDITasks editorState iterationState) -> (GroupActionGenFunc GAction))
	!((globalState,EditorCollection editorState) -> MenuDefinition)
	->
	Task Void | iTask globalState & iTask editorState & iTask iterationState

// A collection of tasks for dealing with editors within an MDI application.
:: MDITasks editorState iterationState = {
	/**
	* Creates a new editor.
	*
	* @param An initial editor state, stored as long as the editor task is running
	* @param The editor task using a reference to the editor's state
	* @return The created editor task
	*/
	createEditor :: MDICreateEditor editorState,
	
	/**
	* Iterates over all editors using an accumulator tasks to transform an state.
	*
	* @param The initial value of the accumulated state
	* @param The accumulator task, getting the current state and a reference to the current editor's state and returning a new value of the state
	* @param The final value of the accumulated state
	*/
	iterateEditors :: MDIIterateEditors editorState iterationState,
	
	/**
	* Check if an editor for which a given predicate holds exists and a reference to its state.
	*
	* @param The predicate on the editor state
	* @return Nothing if the predicate holds for no editor; A refenrece to the first editor's state for which the predicate holds
	*/
	existsEditor :: MDIExistsEditor editorState
	}
	
:: MDICreateEditor editorState					:== editorState ((EditorId editorState) (SymmetricShared editorState) -> Task Void) -> Task Void
:: MDIIterateEditors editorState iterationState :== iterationState (iterationState (SymmetricShared editorState) -> Task iterationState) -> Task iterationState
:: MDIExistsEditor editorState					:== (editorState -> Bool) -> Task (Maybe (EditorId editorState))
	
:: EditorId est :== Int
:: EditorCollection est :== Map (EditorId est) est*/
