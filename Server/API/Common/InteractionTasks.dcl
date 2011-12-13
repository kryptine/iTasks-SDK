definition module InteractionTasks

import CoreTasks

/**
* Defines a view on the data model of interaction tasks. 
*/
:: ViewOn l r w	= E.v:	About			!v									& iTask v	//* additional information independent from the data model the interaction task works on
				| E.v:	EnterView		!(SetFunc l r w v)					& iTask v	//* a view to put information into the data model
				| E.v:	UpdateView		!(!GetFunc l r v, !SetFunc l r w v)	& iTask v	//* a view to update the data model
				| E.v:	DisplayView		!(GetFunc l r v)					& iTask v	//* a view to show the data model
				|		UpdateTrigger	!String !(UpdateFunc l r w)						//* a trigger (typically a button) used to update the data model
/**
* Defines how to get a view from the data model.
*/
:: GetFunc l r v	= GetLocal			!(l		-> v) //* a get function on the local part of the data model
					| GetShared			!(r		-> v) //* a get function on the shared part of the data model
					| GetCombined		!(l r	-> v) //* a get function on both parts of the data model

/**
* Defines how to put view data back into the data model.
*/					
:: SetFunc l r w v	= SetLocal			!(v l r -> l)						//* a putback function to put information into the local data model
					| SetShared			!(v l r -> w)						//* a putback function to put information into the shared data model
					| SetCombined		!(v l r -> (!Maybe l,!Maybe w)) 	//* a putback function to possibly put information into the local/shared data model
/**
* Defines how to update the data model.
*/						
:: UpdateFunc l r w	= UpdateLocal		!(l -> l)							//* a function updating the local data model
					| UpdateShared		!(r -> w)							//* a function update the shared data model
					| UpdateCombined	!(l r -> (!Maybe l, Maybe w))		//* a function possibly updating the local/shared data model

:: LocalViewOn a :== ViewOn a Void Void

/*** General input/update/output tasks ***/

/**
* Ask the user to enter information.
*
* @param Description:		A description of the task to display to the user
*							@default ""
* @param Views:				Interaction views; only putback parts of Views are used, Gets are ignored; if no putback is defined the id putback with v = w is used
*							@default [] @gin-visible False
*
* @return					Value entered by the user
* 
* @gin-icon page_white
*/
enterInformation :: !d ![LocalViewOn m] -> Task m | descr d & iTask m

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
updateInformation :: !d ![LocalViewOn m] m -> Task m | descr d & iTask m

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
viewInformation :: !d ![LocalViewOn m] !m -> Task m | descr d & iTask m

/**
* Ask the user to enter local information and information which is written to a shared.
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; only putback parts of Views are used, Gets are ignored; if no putback is defined the id putback with v = w is used for the local and shared part
*                           @default [] @gin-visible False
* @param Shared:			Reference to the shared state to which the entered information is written
*
* @return					Last value of the shared state to which the user added information
* @throws					SharedException
* 
* @gin-icon page_white
*/
enterSharedInformation :: !d ![ViewOn l r w] !(ReadWriteShared r w) -> Task (r,l) | descr d & iTask l & iTask r & iTask w

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
* @return 					Last value of the shared state the user updated
* @throws					SharedException
* 
* @gin-icon page_edit
*/
updateSharedInformation :: !d ![ViewOn l r w] !(ReadWriteShared r w) l -> Task (r,l) | descr d & iTask l & iTask r & iTask w

/**
* Show a local and shared state.
*
* @param Description:		A description of the task to display to the user
* @param Views:				Interaction views; only get parts of Views are used, Putbacks are ignored; if no get is defined the id get is used for the local and shared part
* @param Shared:			Reference to the shared state to monitor
* @param Local:				The local data shown to the user
*
* @return					Last value of the monitored state
* @throws					SharedException
* 
* @gin-icon monitor
*/
viewSharedInformation :: !d ![ViewOn l r w] !(ReadWriteShared r w) !l -> Task (r,l) | descr d & iTask l & iTask r & iTask w


/*** Special tasks for choices ***/

:: ChoiceView choiceType o	= E.v: ChoiceContext	!v							& iTask v
							| E.v: ChoiceView		!(!choiceType, !(o -> v))	& iTask v

/**
* Ask the user to select one item from a list of options.
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; only the first ShowView has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
*                           @default [] @gin-visible False
* @param Choice options:	A list of options the user can choose from
*                           @default []
*
* @return					The option chosen by the user
* 
* @gin-icon choice
*/
//ListChoice (a -> [o], o -> v)
//TreeChoice (a -> Tree o, o -> v)
//enterChoice :: !d ![ChoiceView ChoiceType o] !a -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)

enterChoice :: !d ![ChoiceView ChoiceType o] !(container o) -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)

/**
* Ask the user to select one item from a list of options with already one option pre-selected.
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; only the first ShowView has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
*                           @default [] @gin-visible False
* @param Choice options:	A list of options the user can choose from
*                           @default []
* @param Selection:			The pre-selected item; if it is not member of the options list no options is pre-selected
*
* @return 					The option chosen by the user
* 
* @gin-icon choice
*/
updateChoice :: !d ![ChoiceView ChoiceType o] !(container o) o -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)

/**
* Ask the user to select one item from a list of shared options.
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; only the first ShowView has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
*                           @default [] @gin-visible False
* @param Shared:			Reference to the shared state including the options the user can choose from
*
* @return 					The option chosen by the user
* @throws					SharedException
* 
* @gin-icon choice
*/
enterSharedChoice :: !d ![ChoiceView ChoiceType o] !(ReadWriteShared (container o) w) -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)

/**
* Ask the user to select one item from a list of shared options with already one option pre-selected.
*
* @param Description:		A description of the task to display to the user
*                           @default ""
* @param Views:				Interaction views; only the first ShowView has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
*                           @default [] @gin-visible False
* @param Shared:			Reference to the shared state including the options the user can choose from
* @param Selection:			The pre-selected item; if it is not member of the options list no options is pre-selected
*
* @return 					The option chosen by the user
* @throws					SharedException
* 
* @gin-icon choice
*/
updateSharedChoice :: !d ![ChoiceView ChoiceType o] !(ReadWriteShared (container o) w) o -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)

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
enterMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(container o) -> Task [o] | descr d & OptionContainer container & iTask o & iTask (container o)

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
updateMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(container o) [o] -> Task [o] | descr d & OptionContainer container & iTask o & iTask (container o)

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
enterSharedMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(ReadWriteShared (container o) w) -> Task [o] | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)

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
updateSharedMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(ReadWriteShared (container o) w) [o] -> Task [o] | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)

/**
* Wait for a share to match a certain predicate
*
* @param Description:		A description of the task to display to the user
* @param Predicate:			A predicate to test when to continue. The task completes as soon as the predicate is true
* @param Shared:			Reference to the shared state to wait for
*
* @return					The value of the shared when the predicate becomes true
*/
wait :: d (r -> Bool) (ReadWriteShared r w) -> Task r | descr d & iTask r & iTask w

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
