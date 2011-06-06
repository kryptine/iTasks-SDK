definition module InteractionTasks

from StdFunc	import id, const
import CoreTasks

:: ViewOn r w		= E.v: About	!v						& iTask v // additional information independent from the data model the interaction task works on
					| E.v: View		!(!r -> v,!v r -> w)	& iTask v // a complete lens on the data model, making it possible to update it
					| E.v: Get		!(r -> v)				& iTask v // a get function on the data model, showing it
					| E.v: Putback	!(v r -> w)				& iTask v // a putback function to put information into the data model
							
:: LocalViewOn a :== ViewOn a a

/*** General input/update/output tasks ***/

/**
* Ask the user to enter information.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only putback parts of Views are used, Gets are ignored; if no putback is defined the id putback with v = w is used
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
* @param Options:			Interaction options; if no view is defined a default view with the id lens is used
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
* @param Options:			Interaction options; only get parts of Views are used, Putbacks are ignored; if no get is defined the id get is used
* @param Data model:		The data shown to the user
*
* @return					Value shown to the user, the value is not modified
* 
* @gin-icon information
*/
showInformation :: !d ![LocalViewOn m] !m -> Task m | descr d & iTask m

/**
* Ask the user to enter information which is written to a shared.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only putback parts of Views are used, Gets are ignored; if no putback is defined the id putback with v = w is used
* @param Shared:			Reference to the shared state to which the entered information is written
*
* @return					Last value of the shared state to which the user added information
* @throws					SharedException
* 
* @gin-icon page_white
*/
enterSharedInformation :: !d ![ViewOn r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w

/**
* Ask the user to update predefined shared information.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; if no view is defined the value of the shared state (r) is shown to the user
* @param Shared:			Reference to the shared state to update
*
* @return 					Last value of the shared state the user updated
* @throws					SharedException
* 
* @gin-icon page_edit
*/
updateSharedInformation :: !d ![ViewOn r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w

/**
* Monitor a shared state.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only get parts of Views are used, Putbacks are ignored; if no get is defined the id get is used
* @param Shared:			Reference to the shared state to monitor
*
* @return					Last value of the monitored state
* @throws					SharedException
* 
* @gin-icon monitor
*/
monitor :: !d ![ViewOn r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w


/*** Special tasks for choices ***/

/**
* Ask the user to select one item from a list of options.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only the first Get/get part of View has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
* @param Choice options:	A list of options the user can choose from
*
* @return					The option chosen by the user
* 
* @gin-icon choice
*/
enterChoice :: !d ![LocalViewOn o] ![o] -> Task o | descr d & iTask o

/**
* Ask the user to select one item from a list of options with already one option pre-selected.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only the first Get/get part of View has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
* @param Choice options:	A list of options the user can choose from
* @param Selection:			The pre-selected item; if it is not member of the options list no options is pre-selected
*
* @return 					The option chosen by the user
* 
* @gin-icon choice
*/
updateChoice :: !d ![LocalViewOn o] ![o] o -> Task o | descr d & iTask o

/**
* Ask the user to select one item from a list of shared options.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only the first Get/get part of View has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
* @param Shared:			Reference to the shared state including the options the user can choose from
*
* @return 					The option chosen by the user
* @throws					SharedException
* 
* @gin-icon choice
*/
enterSharedChoice :: !d ![ViewOn o w] !(Shared [o] w) -> Task o | descr d & iTask o & iTask w

/**
* Ask the user to select one item from a list of shared options with already one option pre-selected.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only the first Get/get part of View has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
* @param Shared:			Reference to the shared state including the options the user can choose from
* @param Selection:			The pre-selected item; if it is not member of the options list no options is pre-selected
*
* @return 					The option chosen by the user
* @throws					SharedException
* 
* @gin-icon choice
*/
updateSharedChoice :: !d ![ViewOn o w] !(Shared [o] w) o -> Task o | descr d & iTask o & iTask w

/**
* Ask the user to select a number of items from a list of options
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only the first Get/get part of View has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
* @param Choice options:	A list of options the user can choose from
*
* @return					The options chosen by the user
* 
* @gin-icon choice
*/
enterMultipleChoice :: !d ![LocalViewOn o] ![o] -> Task [o] | descr d & iTask o

/**
* Ask the user to select a number of items from a list of options with already a number of options pre-selected.
*
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only the first Get/get part of View has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
* @param Choice options:	A list of options the user can choose from
* @param Selection:			The pre-selected items; items which are not member of the option list are ignored
*
* @return 					The options chosen by the user
* 
* @gin-icon choice
*/
updateMultipleChoice :: !d ![LocalViewOn o] ![o] [o] -> Task [o] | descr d & iTask o

/**
* Ask the user to select a number of items from a list of shared options.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only the first Get/get part of View has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
* @param Shared:			Reference to the shared state including the options the user can choose from
*
* @return 					The options chosen by the user
* @throws					SharedException
* 
* @gin-icon choice
*/
enterSharedMultipleChoice :: !d ![ViewOn o w] !(Shared [o] w) -> Task [o] | descr d & iTask o & iTask w

/**
* Ask the user to select one item from a list of shared options with already a number of options pre-selected.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Interaction options; only the first Get/get part of View has an effect, it is used to map all options (o) to a view type (v); if no get is defined the id get is used
* @param Shared:			Reference to the shared state including the options the user can choose from
* @param Selection:			The pre-selected items; items which are not member of the option list are ignored
*
* @return 					The options chosen by the user
* @throws					SharedException
* 
* @gin-icon choice
*/
updateSharedMultipleChoice :: !d ![ViewOn o w] !(Shared [o] w) [o] -> Task [o] | descr d & iTask o & iTask w


/*** Special wait tasks ***/

/**
* Creates a task which blocks a workflow until a specified time.
*
* @param Time: The specified time at which the task should complete
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
*
* @return The date to wait for
* 
* @gin-icon date_go
*/
waitForDate		:: !Date			-> Task Date
/**
* Task completes after specified amount of time has passed
* since the creation of the task.
*
* @param Time: The time to wait before the task should complete
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
* Ask the user to choose an action. The list of actions is calculated dynamically.
*
* @param Action function:	A function generating a list of actions the user can choose from. Each actions yields the given result if it's chosen & result is present (Just). Otherwise (Nothing) action is disabled.
* @param Shared:			Reference to a shared state the actions depend on
*
* @return 					Value associated with chosen action
* @throws					SharedException
* 
* @gin False
*/						
chooseActionDyn :: !(Shared r w) !(r -> [(!Action,Maybe a)]) -> Task a | iTask a & iTask r & iTask w

/**
* A derived version of 'interact' which only uses a local state.
*
* @param Description:			A description of the task to display to the user
* @param Terminator function:	A function (on the current local state) dynamically generating the interaction parts shown to the user (parts can change the local state (l))
* @param Local state:			The initial local state
*
* @return						The last value of the local state
* 
* @gin False
*/
interactLocal :: !d !(l -> [InteractionPart l]) l -> Task l | descr d & iTask l