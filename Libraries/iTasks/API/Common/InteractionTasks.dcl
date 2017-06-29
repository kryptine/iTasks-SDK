definition module iTasks.API.Common.InteractionTasks

import iTasks.API.Core.Tasks

from iTasks.API.Core.Types import :: Date, :: Time, :: DateTime, :: Action
from Data.Functor import class Functor
from iTasks.UI.Editor.Builtin import :: ChoiceText, :: ChoiceGrid, :: ChoiceNode 
from iTasks.SDS.Definition import :: Shared

/*** General input/update/output tasks ***/

:: ViewOption a 		= E.v: ViewAs 	    (a -> v)                       & iTask v
						| E.v: ViewUsing 	(a -> v) (Editor v)            & iTask v //Use a custom editor to view the data

:: EnterOption a		= E.v: EnterAs      (v -> a)                       & iTask v
						| E.v: EnterUsing 	(v -> a) (Editor v)            & iTask v //Use a custom editor to enter the data

:: UpdateOption a b		= E.v: UpdateAs     (a -> v) (a v -> b)	           & iTask v
						| E.v: UpdateUsing  (a -> v) (a v -> b) (Editor v) & iTask v //Use a custom editor to enter the data
                        //When using an update option for a task that uses a shared data source
                        //you can use UpdateWithShared instead of UpdateWith which allows you
                        //to specify how the view must be updated when both the share changed and
                        //the user changed the view simultaneously. This conflict resolution function
                        //is applied before the new 'b' is generated from the view ('v') value
                        | E.v: UpdateSharedAs (a -> v) (a v -> b) (v v -> v)  & iTask v 

//Selection in arbitrary containers (explicit identification is needed)
:: SelectOption c s     = SelectInDropdown   (c -> [ChoiceText]) (c [Int] -> [s])
     					| SelectInCheckGroup (c -> [ChoiceText]) (c [Int] -> [s])
     					| SelectInList       (c -> [ChoiceText]) (c [Int] -> [s])
     					| SelectInGrid       (c -> ChoiceGrid)   (c [Int] -> [s])
     					| SelectInTree       (c -> [ChoiceNode]) (c [Int] -> [s])
	
//Choosing from lists
:: ChoiceOption o	    = E.v: ChooseFromDropdown (o -> v)   & iTask v
						| E.v: ChooseFromCheckGroup (o -> v) & iTask v
						| E.v: ChooseFromList (o -> v)       & iTask v
						| E.v: ChooseFromGrid (o -> v)       & iTask v

/*** General input/update/output tasks ***/

/**
* Ask the user to enter information.
*
* @param Description:		A description of the task to display to the user
* @param Views:				Views
*
* @return					Value entered by the user
*/
enterInformation :: !d ![EnterOption m] -> Task m | toPrompt d & iTask m

/**
* Ask the user to update predefined information. 
*
* @param Description:		A description of the task to display to the user
* @param Views:				Interaction views; if no view is defined a default view with the id lens is used
* @param Data model:		The data updated by the user
*
* @return					Value updated by the user
*/
updateInformation :: !d ![UpdateOption m m] m -> Task m | toPrompt d & iTask m 

/**
* Show information to the user. 
*
* @param Description:		A description of the task to display to the user
* @param Views:				Interaction views; only get parts of Views are used, Putbacks are ignored; if no get is defined the id get is used
* @param Data model:		The data shown to the user
*
* @return					Value shown to the user, the value is not modified
*/
viewInformation :: !d ![ViewOption m] !m -> Task m | toPrompt d & iTask m

/**
* Ask the user to update predefined local and shared information.
*
* @param Description:		A description of the task to display to the user
* @param Views:				Interaction views; if no view is defined & w = r a default view with the id lens is used, if r <> w the value of the shared state (r) is shown to the user; the default for the local data is always the id lens
* @param Shared:			Reference to the shared state to update
* @param Local:				The local data updated by the user

* @return 					Current value of the shared thats being modified and local modified copy
*/
updateSharedInformation :: !d ![UpdateOption r w] !(ReadWriteShared r w) -> Task r | toPrompt d & iTask r & iTask w

/**
* Show a shared value.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Views options
* @param Shared:			Reference to the shared state to monitor
*
* @return					Last value of the monitored state
*/
viewSharedInformation :: !d ![ViewOption r] !(ReadWriteShared r w) -> Task r | toPrompt d & iTask r

/*** Special tasks for a mix of manipulating shared and local information ***/

/**
* Update a local value, making use of shared information.
*/
updateInformationWithShared :: !d ![UpdateOption (r,m) m] !(ReadWriteShared r w) m -> Task m | toPrompt d & iTask r & iTask m

/**
* General selection with explicit identification in arbitrary containers
*/

//Options: local, selection: local
editSelection :: !d !Bool !(SelectOption c a) c [Int] -> Task [a] | toPrompt d & iTask a

//Options: shared, selection: local
editSelectionWithShared :: !d !Bool !(SelectOption c a) (ReadWriteShared c w) (c -> [Int]) -> Task [a] | toPrompt d & iTask c & iTask a 

//Options: local, selection: shared
editSharedSelection :: !d !Bool !(SelectOption c a) c (Shared [Int]) -> Task [a] | toPrompt d & iTask c & iTask a 

//Options: shared, selection: shared
editSharedSelectionWithShared :: !d !Bool !(SelectOption c a) (ReadWriteShared c w) (Shared [Int]) -> Task [a] | toPrompt d & iTask c & iTask a 

/**
* More specific selection from lists
*/
editChoice                           :: !d ![ChoiceOption a] ![a] (Maybe a) -> Task a | toPrompt d & iTask a
editChoiceAs                         :: !d ![ChoiceOption o] ![o] !(o -> a) (Maybe a) -> Task a | toPrompt d & iTask o & iTask a
editMultipleChoice                   :: !d ![ChoiceOption a] ![a] [a] -> Task [a] | toPrompt d & iTask a
editMultipleChoiceAs                 :: !d ![ChoiceOption o] ![o] !(o -> a) [a] -> Task [a] | toPrompt d & iTask o & iTask a

enterChoice                          :: !d ![ChoiceOption a] ![a] -> Task a | toPrompt d & iTask a
enterChoiceAs                        :: !d ![ChoiceOption o] ![o] !(o -> a) -> Task a | toPrompt d & iTask o & iTask a
enterMultipleChoice                  :: !d ![ChoiceOption a] ![a] -> Task [a] | toPrompt d & iTask a
enterMultipleChoiceAs                :: !d ![ChoiceOption o] ![o] !(o -> a) -> Task [a] | toPrompt d & iTask o & iTask a

updateChoice                         :: !d ![ChoiceOption a] ![a] a -> Task a | toPrompt d & iTask a
updateChoiceAs                       :: !d ![ChoiceOption o] ![o] !(o -> a) a -> Task a | toPrompt d & iTask o & iTask a
updateMultipleChoice                 :: !d ![ChoiceOption a] ![a] [a] -> Task [a] | toPrompt d & iTask a
updateMultipleChoiceAs               :: !d ![ChoiceOption o] ![o] !(o -> a) [a] -> Task [a] | toPrompt d & iTask o & iTask a

editChoiceWithShared                 :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Maybe a) -> Task a | toPrompt d & iTask a & iTask w
editChoiceWithSharedAs               :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Maybe a) -> Task a | toPrompt d & iTask o & iTask w & iTask a
editMultipleChoiceWithShared         :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) [a] -> Task [a] | toPrompt d & iTask a & iTask w
editMultipleChoiceWithSharedAs       :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) [a] -> Task [a] | toPrompt d & iTask o & iTask w & iTask a

enterChoiceWithShared                :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) -> Task a | toPrompt d & iTask a & iTask w
enterChoiceWithSharedAs              :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) -> Task a | toPrompt d & iTask o & iTask w & iTask a
enterMultipleChoiceWithShared        :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) -> Task [a] | toPrompt d & iTask a & iTask w
enterMultipleChoiceWithSharedAs      :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) -> Task [a] | toPrompt d & iTask o & iTask w & iTask a

updateChoiceWithShared               :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) a -> Task a | toPrompt d & iTask a & iTask w
updateChoiceWithSharedAs             :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) a -> Task a | toPrompt d & iTask o & iTask w & iTask a
updateMultipleChoiceWithShared       :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) [a] -> Task [a] | toPrompt d & iTask a & iTask w
updateMultipleChoiceWithSharedAs     :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) [a] -> Task [a] | toPrompt d & iTask o & iTask w & iTask a

editSharedChoice                     :: !d ![ChoiceOption a] ![a] (Shared (Maybe a)) -> Task a | toPrompt d & iTask a
editSharedChoiceAs                   :: !d [ChoiceOption o] ![o] !(o -> a) (Shared (Maybe a)) -> Task a | toPrompt d & iTask o & iTask a
editSharedMultipleChoice             :: !d ![ChoiceOption a] ![a] (Shared [a]) -> Task [a] | toPrompt d & iTask a
editSharedMultipleChoiceAs           :: !d [ChoiceOption o] ![o] !(o -> a) (Shared [a]) -> Task [a] | toPrompt d & iTask o & iTask a

editSharedChoiceWithShared           :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Shared (Maybe a)) -> Task a | toPrompt d & iTask a & iTask w
editSharedChoiceWithSharedAs         :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Shared (Maybe a)) -> Task a | toPrompt d & iTask o & iTask w & iTask a
editSharedMultipleChoiceWithShared   :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Shared [a]) -> Task [a] | toPrompt d & iTask a & iTask w
editSharedMultipleChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Shared [a]) -> Task [a] | toPrompt d & iTask o & iTask w & iTask a

/**
* Wait for a share to match a certain predicate
*
* @param Description:		A description of the task to display to the user
* @param Predicate:			A predicate to test when to continue. The task completes as soon as the predicate is true
* @param Shared:			Reference to the shared state to wait for
*
* @return					The value of the shared when the predicate becomes true
*/
wait :: !d (r -> Bool) !(ReadWriteShared r w) -> Task r | toPrompt d & iTask r

/*** Special wait tasks ***/
/**
* Creates a task which blocks a workflow until a specified time.
*
* @param Time: The specified time at which the task should complete
*
* @return The time to wait for
* 
*/
waitForTime		:: !Time			-> Task Time
/**
* Creates a task which blocks a workflow until a specified date.
*
* @param Date: The specified date at which the task should complete
*
* @return The date to wait for
*/
waitForDate		:: !Date			-> Task Date
/**
* Creates a task which blocks a workflow until a specified date and time.
*
* @param DateTime: The specified date and time at which the task should complete
*
* @return The date and time to wait for
*/
waitForDateTime :: !DateTime 		-> Task DateTime
/**
* Task completes after specified amount of time has passed
* since the creation of the task.
*
* @param The time to wait (in seconds before the task should complete
*
* @return The time the timer went off
* 
*/
waitForTimer	:: !Int -> Task DateTime

/*** Special tasks for choosing actions ***/

/**
* Ask the user to choose an action. 
*
* @param Action list:	A list of actions the user can choose from. Each actions yields the given result if it's chosen. 
*
* @return 				Value associated with chosen action
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
crud :: !d !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
        (RWShared () (f r) (f` w))
     -> Task r | toPrompt d & iTask r & iTask (f r) & iTask w & iTask (f` w)

crudWith :: !d ![ChoiceOption r] [EnterOption r] [ViewOption r] [UpdateOption r r]
            !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
            (RWShared () (f r) (f` w))
         -> Task r | toPrompt d & iTask r & iTask (f r) & iTask w & iTask (f` w)
