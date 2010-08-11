definition module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/

import CoreCombinators, TuningCombinators, InteractionTasks
import Either

from Types import :: User (..)

//Grouping composition

// types are similar to PAction but are needed to avoid circular definitions
:: GAction		= GStop | GContinue | GExtend [Task GAction] | GFocus String
:: GOnlyAction	= GOStop | GOContinue | GOExtend [Task Void] | GOFocus String

derive gVisualize	GAction, GOnlyAction
derive gUpdate		GAction, GOnlyAction
derive gError		GAction, GOnlyAction
derive gHint		GAction, GOnlyAction

derive JSONEncode	GAction, GOnlyAction
derive JSONDecode	GAction, GOnlyAction

/**
* Tasks can dynamically add other tasks or stop execution of group.
*
* @param List of initial tasks
*/
dynamicGroup		:: ![Task GAction]									-> Task Void

/**
* Tasks and group-actions can dynamically add other tasks or stop execution of group.
*
* @param List of initial tasks
* @param List of group-actions
*/
dynamicGroupA		:: ![Task GAction] ![GroupAction GAction Void s]	-> Task Void | iTask s

/**
* Only group-actions can dynamically add other tasks or stop execution of group.
*
* @param List of initial tasks
* @param List of group-actions
*/
dynamicGroupAOnly	:: ![Task Void] ![GroupAction GOnlyAction Void s]	-> Task Void | iTask s

(-||-) infixr 3 	:: !(Task a) !(Task a) 	-> Task a 				| iTask a
(||-)  infixr 3		:: !(Task a) !(Task b)	-> Task b				| iTask a & iTask b
(-||)  infixl 3		:: !(Task a) !(Task b)	-> Task a				| iTask a & iTask b

(-&&-) infixr 4 	:: !(Task a) !(Task b) 	-> Task (a,b) 			| iTask a & iTask b

anyTask				:: ![Task a]			-> Task a				| iTask a
allTasks			:: ![Task a]			-> Task [a]				| iTask a
eitherTask			:: !(Task a) !(Task b) 	-> Task (Either a b)	| iTask a & iTask b	

//Parallel composition
orProc 				:: !(Task a) !(Task a) !TaskParallelType -> Task a 	 	| iTask a
andProc 			:: !(Task a) !(Task b) !TaskParallelType -> Task (a,b) 	| iTask a & iTask b
anyProc 			:: ![Task a] 		   !TaskParallelType -> Task a 	 	| iTask a
allProc 			:: ![Task a] 		   !TaskParallelType -> Task [a] 	| iTask a

//Legacy.. should be removed
oldParallel :: !String !([a] -> Bool) ([a] -> b) ([a] -> b) ![Task a] -> Task b | iTask a & iTask b 

//Task composition for optional values
(>>?)	infixl 1	:: !(Task (Maybe a)) !(a -> Task (Maybe b))	-> Task (Maybe b) 		| iTask a & iTask b
(-&?&-)	infixr 4	:: !(Task (Maybe a)) !(Task (Maybe b)) 		-> Task (Maybe (a,b)) 	| iTask a & iTask b

//Post processing of results
ignoreResult		:: !(Task a) 								-> Task Void			| iTask a
transformResult 	:: !(a -> b) !(Task a)						-> Task b				| iTask a & iTask b

//Synonym for (return Void)
stop				:: Task Void

//Random choice
randomChoice		:: ![a]										-> Task a				| iTask a

//Task delegation
(@:) infix 3		:: !User !(Task a) -> Task a | iTask a

/* Handling recursion and loops:
repeatTask		:: repeat Task until predicate is valid
(<|)			:: repeat task (recursively) as long as predicate does not hold, and give error message otherwise
*/
repeatTask		:: !(a -> Task a) !(a -> Bool) a 			-> Task a					| iTask a
(<|)  infixl 6 	:: !(Task a)  !(a -> (Bool, [HtmlTag])) 	-> Task a 					| iTask a

/**
* Combinator for creating Multiple Document Interface (MDI) applications.
*
* @param An initial state for global application data
* @param A function generating global application group actions.
*        The first parameter is a reference to the global state store.
*        The second parameter is a collection of tasks for dealing with editors.
*/
mdiApplication :: !globalState !((DBid globalState) (MDITasks editorState iterationState) -> [GroupAction GAction Void globalState]) -> Task Void | iTask, SharedVariable globalState & iTask, SharedVariable editorState & iTask iterationState

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
	
:: MDICreateEditor editorState					:== editorState ((DBid editorState) -> Task Void) -> Task GAction
:: MDIIterateEditors editorState iterationState :== iterationState (iterationState (DBid editorState) -> Task iterationState) -> Task iterationState
:: MDIExistsEditor editorState					:== (editorState -> Bool) -> Task (Maybe (DBid editorState))