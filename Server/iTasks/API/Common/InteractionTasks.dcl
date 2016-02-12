definition module iTasks.API.Common.InteractionTasks

import iTasks.API.Core.Tasks
from iTasks.API.Core.Types import :: ChoiceTree, :: ChoiceTreeValue, :: Date, :: Time, :: DateTime, :: Action
from Data.Functor import class Functor

//Option types for customizing interaction
:: ViewOption a 		= E.v: ViewWith 	(a -> v)			& iTask v
:: EnterOption a		= E.v: EnterWith	(v -> a)			& iTask v
:: UpdateOption a b		= E.v: UpdateWith	      (a -> v) (a v -> b)	          & iTask v
                        //When using an update option for a task that uses a shared data source
                        //you can use UpdateWithShared instead of UpdateWith which allows you
                        //to specify how the view must be updated when both the share changed and
                        //the user changed the view simultaneously. This conflict resolution function
                        //is applied before the new 'b' is generated from the view ('v') value
                        | E.v: UpdateWithShared   (a -> v) (a v -> b) (v v -> v)  & iTask v 

:: ChoiceOption o       = E.v: ChooseWith (ChoiceType o v)      & iTask v
:: ChoiceType o v	    = AutoChoice (o -> v)
						| ChooseFromComboBox (o -> v)
						| ChooseFromRadioButtons (o -> v)
						| ChooseFromList (o -> v)
						| ChooseFromGrid (o -> v)
						| ChooseFromTree ([(Int,o)] [ChoiceTreeValue] -> [ChoiceTree v])
					
:: MultiChoiceOption a	= E.v: ChooseMultipleWith	MultiChoiceType	    (a -> v) & iTask v
:: MultiChoiceType		= AutoMultiChoice
						| ChooseFromCheckBoxes

/*** General input/update/output tasks ***/

/**
* Ask the user to enter information.
*
* @param Description:		A description of the task to display to the user
*							@default ""
* @param Views:				Views
*							@default [] @gin-visible False
*
* @return					Value entered by the user
* 
* @gin-icon page_white
*/
enterInformation :: !d ![EnterOption m] -> Task m | descr d & iTask m

/**
* Ask the user to update predefined information. 
*
* @param Description:		A description of the task to display to the user
*							@default ""
* @param Views:				Interaction views; if no view is defined a default view with the id lens is used
*							@default [] @gin-visible False
* @param Data model:		The data updated by the user
*
* @return					Value updated by the user
* 
* @gin-icon page_edit
*/
updateInformation :: !d ![UpdateOption m m] m -> Task m | descr d & iTask m

/**
* Show information to the user. 
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; only get parts of Views are used, Putbacks are ignored; if no get is defined the id get is used
*                           @default [] @gin-visible False
* @param Data model:		The data shown to the user
*							@default ""							
*
* @return					Value shown to the user, the value is not modified
* 
* @gin-icon information
*/
viewInformation :: !d ![ViewOption m] !m -> Task m | descr d & iTask m

/**
* Ask the user to update predefined local and shared information.
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; if no view is defined & w = r a default view with the id lens is used, if r <> w the value of the shared state (r) is shown to the user; the default for the local data is always the id lens
*                           @default [] @gin-visible False
* @param Shared:			Reference to the shared state to update
* @param Local:				The local data updated by the user
*
* @return 					Current value of the shared thats being modified and local modified copy
* @throws					SharedException
* 
* @gin-icon page_edit
*/
updateSharedInformation :: !d ![UpdateOption r w] !(ReadWriteShared r w) -> Task w | descr d & iTask r & iTask w

/**
* Show a shared value[].
*
* @param Description:		A description of the task to display to the user
* @param Options:			Views options
* @param Shared:			Reference to the shared state to monitor
*
* @return					Last value of the monitored state
* @throws					SharedException
* 
* @gin-icon monitor
*/
viewSharedInformation :: !d ![ViewOption r] !(ReadWriteShared r w) -> Task r | descr d & iTask r

/*** Special tasks for a mix of manipulating shared and local information ***/

/**
* Update a local value, making use of shared information.
*/
updateInformationWithShared :: !d ![UpdateOption (r,m) m] !(ReadWriteShared r w) m -> Task m | descr d & iTask r & iTask m

/*** Special tasks for choices ***/

/**
* Select one item from a list of options.
*/

editChoice :: !d ![ChoiceOption a] ![a] (Maybe a) -> Task a | descr d & iTask a
editChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) (Maybe a) -> Task a | descr d & iTask o & iTask a

enterChoice :: !d ![ChoiceOption a] ![a] -> Task a | descr d & iTask a
enterChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) -> Task a | descr d & iTask o & iTask a

updateChoice :: !d ![ChoiceOption a] ![a] a -> Task a | descr d & iTask a
updateChoiceAs :: !d ![ChoiceOption o] ![o] !(o -> a) a -> Task a | descr d & iTask o & iTask a

editChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Maybe a) -> Task a | descr d & iTask a & iTask w
editChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Maybe a) -> Task a | descr d & iTask o & iTask w & iTask a

enterChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) -> Task a | descr d & iTask a & iTask w
enterChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) -> Task a | descr d & iTask o & iTask w & iTask a

updateChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) a -> Task a | descr d & iTask a & iTask w
updateChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) a -> Task a | descr d & iTask o & iTask w & iTask a

editSharedChoice :: !d ![ChoiceOption a] ![a] (Shared (Maybe a)) -> Task a | descr d & iTask a
editSharedChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) (Shared (Maybe a)) -> Task a | descr d & iTask o & iTask a

editSharedChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Shared (Maybe a)) -> Task a | descr d & iTask a & iTask w
editSharedChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Shared (Maybe a)) -> Task a | descr d & iTask o & iTask w & iTask a

/**
* Ask the user to select a number of items from a list of options
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; only the first ShowView has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
*                           @default [] @gin-visible False
* @param Choice options:	A list of options the user can choose from
*                           @default []
* 
* @return					The options chosen by the user
* 
* @gin-icon choice
*/
enterMultipleChoice :: !d ![MultiChoiceOption o] ![o] -> Task [o] | descr d & iTask o

/**
* Ask the user to select a number of items from a list of options with already a number of options pre-selected.
*
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; only the first ShowView has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
*                           @default [] @gin-visible False
* @param Choice options:	A list of options the user can choose from
*                           @default []
* @param Selection:			The pre-selected items; items which are not member of the option list are ignored
*                           @default []
*
* @return 					The options chosen by the user
* 
* @gin-icon choice
*/
updateMultipleChoice :: !d ![MultiChoiceOption o] ![o] [o] -> Task [o] | descr d & iTask o

/**
* Ask the user to select a number of items from a list of shared options.
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; only the first ShowView has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
*                           @default []
* @param Shared:			Reference to the shared state including the options the user can choose from
*
* @return 					The options chosen by the user
* @throws					SharedException
* 
* @gin-icon choice
*/
enterSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared [o] w) -> Task [o] | descr d & iTask o & iTask w

/**
* Ask the user to select one item from a list of shared options with already a number of options pre-selected.
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; only the first ShowView has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
*                           @default []
* @param Shared:			Reference to the shared state including the options the user can choose from
*                           @default []
* @param Selection:			The pre-selected items; items which are not member of the option list are ignored
*                           @default []
*
* @return 					The options chosen by the user
* @throws					SharedException
* 
* @gin-icon choice
*/
updateSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared [o] w) [o] -> Task [o] | descr d & iTask o & iTask w

/**
* Wait for a share to match a certain predicate
*
* @param Description:		A description of the task to display to the user
* @param Predicate:			A predicate to test when to continue. The task completes as soon as the predicate is true
* @param Shared:			Reference to the shared state to wait for
*
* @return					The value of the shared when the predicate becomes true
*/
wait :: !d (r -> Bool) !(ReadWriteShared r w) -> Task r | descr d & iTask r

/*** Special wait tasks ***/
/**
* Creates a task which blocks a workflow until a specified time.
*
* @param Time: The specified time at which the task should complete
*			   @default {hour = 0, min = 0, sec = 0}
*
* @return The time to wait for
* 
* @gin-icon clock_go
*/
waitForTime		:: !Time			-> Task Time
/**
* Creates a task which blocks a workflow until a specified date.
*
* @param Date: The specified date at which the task should complete
*			   @default {day = 1, month = 1, year = 2011}
*
* @return The date to wait for
* 
* @gin-icon date_go
*/
waitForDate		:: !Date			-> Task Date
/**
* Creates a task which blocks a workflow until a specified date and time.
*
* @param DateTime: The specified date and time at which the task should complete
*			       @default DateTime {day = 1, month = 1, year = 2011} {hour = 0, min = 0, sec = 0}
*
* @return The date and time to wait for
* 
* @gin-icon date_go
*/
waitForDateTime :: !DateTime 		-> Task DateTime
/**
* Task completes after specified amount of time has passed
* since the creation of the task.
*
* @param Time: The time to wait before the task should complete
*			   @default {hour = 0, min = 0, sec = 0}
*
* @return The time the timer went off
* 
* @gin-icon clock_go
*/
waitForTimer	:: !Time			-> Task Time

/*** Special tasks for choosing actions ***/

/**
* Ask the user to choose an action. 
*
* @param Action list:	A list of actions the user can choose from. Each actions yields the given result if it's chosen. 
*
* @return 				Value associated with chosen action
* 
* @gin False
*/
chooseAction :: ![(!Action,a)] -> Task a | iTask a

/**
* View data as a title
*/
viewTitle :: !a -> Task a | iTask a

/**
* View shared data as a title
*/
viewSharedTitle :: !(ReadWriteShared r w) -> Task r | iTask r

/**
* Basic Create, Read, Update, Delete (CRUD) editor for a shared collection
*/
crud :: !d !((f r) -> [r]) !(r (f r) -> f w) !(r (f r) -> f w)
        (RWShared () (f r) (f w))
     -> Task r | descr d & iTask r & iTask (f r) & iTask w & iTask (f w)
