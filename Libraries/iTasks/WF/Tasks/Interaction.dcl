definition module iTasks.WF.Tasks.Interaction

import iTasks.WF.Definition
from iTasks.WF.Combinators.Core import :: Action
from iTasks.UI.Editor.Controls import :: ChoiceText, :: ChoiceGrid, :: ChoiceNode
import iTasks.SDS.Definition

from Data.Functor import class Functor

/*** General input/update/output tasks ***/
:: ViewOption a
	= E.v: ViewAs 	    (a -> v) & iTask v
	| E.v: ViewUsing 	(a -> v) (Editor v) & iTask v
	//Common attributes as option
	| ViewWithHint     !String
	| ViewWithTitle    !String
	| ViewWithLabel    !String

:: EnterOption a
	= E.v: EnterAs      (v -> a) & iTask v
	| E.v: EnterUsing 	(v -> a) (Editor v) & iTask v
	//Common attributes as option
	| EnterWithHint     !String
	| EnterWithTitle    !String
	| EnterWithLabel    !String

:: UpdateOption a
	= E.v: UpdateAs     (a -> v) (a v -> a)	& iTask v
	| E.v: UpdateUsing  (a -> v) (a v -> a) (Editor v) & iTask v
	//Common attributes as option
	| UpdateWithHint     !String
	| UpdateWithTitle    !String
	| UpdateWithLabel    !String

//When using an update option for a task that uses a shared data source
//you can use UpdateWithShared instead of UpdateWith which allows you
//to specify how the view must be updated when both the share changed and
//the user changed the view simultaneously. This conflict resolution function
//is applied before the new 'b' is generated from the view ('v') value
:: UpdateSharedOption a b 
	= E.v: UpdateSharedAs (a -> v) (a v -> b) (v v -> v) & iTask v
    | E.v: UpdateSharedUsing (a -> v) (a v -> b) (v v -> v) (Editor v) & iTask v
	//Common attributes as option
	| UpdateSharedWithHint     !String
	| UpdateSharedWithTitle    !String
	| UpdateSharedWithLabel    !String

//Selection in arbitrary containers (explicit identification is needed)
:: SelectOption c s
	= SelectInDropdown   (c -> [ChoiceText]) (c [Int] -> [s])
    | SelectInCheckGroup (c -> [ChoiceText]) (c [Int] -> [s])
    | SelectInList       (c -> [ChoiceText]) (c [Int] -> [s])
    | SelectInGrid       (c -> ChoiceGrid)   (c [Int] -> [s])
    | SelectInTree       (c -> [ChoiceNode]) (c [Int] -> [s])
	| E.v: SelectUsing   (c -> v) (c [Int] -> [s]) (Editor (v, [Int])) & iTask v
	//Common attributes as option
	| SelectWithHint     !String
	| SelectWithTitle    !String
	| SelectWithLabel    !String
	| SelectMultiple     !Bool

//Choosing from lists
:: ChoiceOption o
	= E.v: ChooseFromDropdown (o -> v)   & iTask v
	| E.v: ChooseFromCheckGroup (o -> v) & iTask v
	| E.v: ChooseFromList (o -> v)       & iTask v
	| E.v: ChooseFromGrid (o -> v)       & iTask v
	//Common attributes as option
	| ChooseWithHint     !String
	| ChooseWithTitle    !String
	| ChooseWithLabel    !String

/*** General input/update/output tasks ***/

/**
* Ask the user to enter information.
*
* @param Options:			Customization options
*
* @return					Value entered by the user
*/
enterInformation :: ![EnterOption m] -> Task m | iTask m

/**
* Ask the user to update predefined information.
*
* @param Options:			Customization options
* @param Data model:		The data updated by the user
*
* @return					Value updated by the user
*/
updateInformation :: ![UpdateOption m] m -> Task m | iTask m

/**
* Allow the user to view some information.
*
* @param Options:			Customization options
* @param Data model:		The data shown to the user
*
* @return					Value shown to the user, the value is not modified
*/
viewInformation :: ![ViewOption m] !m -> Task m | iTask m

/**
* Ask the user to update predefined local and shared information.
*
* @param Options:			Customization options
* @param Shared:			Reference to the shared state to update

* @return 					Current value of the shared thats being modified and local modified copy
*/
updateSharedInformation :: ![UpdateSharedOption r w] !(sds () r w) -> Task r | iTask r & iTask w & RWShared sds

/**
* View a shared value.
*
* @param Options:			Customization options
* @param Shared:			Reference to the shared state to monitor
*
* @return					Last value of the monitored state
*/
viewSharedInformation :: ![ViewOption r] !(sds () r w) -> Task r | iTask r & TC w & Registrable sds

/*** Special tasks for a mix of manipulating shared and local information ***/

/**
* Update a local value, making use of shared information.
*/
updateInformationWithShared :: ![UpdateSharedOption (r,m) m] !(sds () r w) m -> Task m | iTask r & iTask m & TC w & RWShared sds

/**
* General selection with explicit identification in arbitrary containers
*/

//Options: local, selection: local
editSelection :: ![SelectOption c a] c [Int] -> Task [a] | iTask a

//Options: shared, selection: local
editSelectionWithShared :: ![SelectOption c a] (sds () c w) (c -> [Int]) -> Task [a] | iTask c & iTask a & TC w & RWShared sds

//Options: local, selection: shared
editSharedSelection :: ![SelectOption c a] c (Shared sds [Int]) -> Task [a] | iTask c & iTask a & RWShared sds

//Options: shared, selection: shared
editSharedSelectionWithShared :: ![SelectOption c a] (sds1 () c w) (Shared sds2 [Int]) -> Task [a] | iTask c & iTask a & TC w & RWShared sds1 & RWShared sds2

/**
* More specific selection from lists
*/
editChoice                           :: ![ChoiceOption a] ![a] (Maybe a) -> Task a | iTask a
editChoiceAs                         :: ![ChoiceOption o] ![o] !(o -> a) (Maybe a) -> Task a | iTask o & iTask a
editMultipleChoice                   :: ![ChoiceOption a] ![a] [a] -> Task [a] | iTask a
editMultipleChoiceAs                 :: ![ChoiceOption o] ![o] !(o -> a) [a] -> Task [a] | iTask o & iTask a

enterChoice                          :: ![ChoiceOption a] ![a] -> Task a | iTask a
enterChoiceAs                        :: ![ChoiceOption o] ![o] !(o -> a) -> Task a | iTask o & iTask a
enterMultipleChoice                  :: ![ChoiceOption a] ![a] -> Task [a] | iTask a
enterMultipleChoiceAs                :: ![ChoiceOption o] ![o] !(o -> a) -> Task [a] | iTask o & iTask a

updateChoice                         :: ![ChoiceOption a] ![a] a -> Task a | iTask a
updateChoiceAs                       :: ![ChoiceOption o] ![o] !(o -> a) a -> Task a | iTask o & iTask a
updateMultipleChoice                 :: ![ChoiceOption a] ![a] [a] -> Task [a] | iTask a
updateMultipleChoiceAs               :: ![ChoiceOption o] ![o] !(o -> a) [a] -> Task [a] | iTask o & iTask a

editChoiceWithShared                 :: ![ChoiceOption a] !(sds () [a] w) (Maybe a) -> Task a | iTask a & TC w & RWShared sds
editChoiceWithSharedAs               :: ![ChoiceOption o] !(sds () [o] w) (o -> a) (Maybe a) -> Task a | iTask o & TC w & iTask a & RWShared sds
editMultipleChoiceWithShared         :: ![ChoiceOption a] !(sds () [a] w) [a] -> Task [a] | iTask a & TC w & RWShared sds
editMultipleChoiceWithSharedAs       :: ![ChoiceOption o] !(sds () [o] w) (o -> a) [a] -> Task [a] | iTask o & TC w & iTask a & RWShared sds

enterChoiceWithShared                :: ![ChoiceOption a] !(sds () [a] w) -> Task a | iTask a & TC w & RWShared sds
enterChoiceWithSharedAs              :: ![ChoiceOption o] !(sds () [o] w) (o -> a) -> Task a | iTask o & TC w & iTask a & RWShared sds
enterMultipleChoiceWithShared        :: ![ChoiceOption a] !(sds () [a] w) -> Task [a] | iTask a & TC w & RWShared sds
enterMultipleChoiceWithSharedAs      :: ![ChoiceOption o] !(sds () [o] w) (o -> a) -> Task [a] | iTask o & TC w & iTask a & RWShared sds

updateChoiceWithShared               :: ![ChoiceOption a] !(sds () [a] w) a -> Task a | iTask a & TC w & RWShared sds
updateChoiceWithSharedAs             :: ![ChoiceOption o] !(sds () [o] w) (o -> a) a -> Task a | iTask o & TC w & iTask a & RWShared sds
updateMultipleChoiceWithShared       :: ![ChoiceOption a] !(sds () [a] w) [a] -> Task [a] | iTask a & TC w & RWShared sds
updateMultipleChoiceWithSharedAs     :: ![ChoiceOption o] !(sds () [o] w) (o -> a) [a] -> Task [a] | iTask o & TC w & iTask a & RWShared sds

editSharedChoice                     :: ![ChoiceOption a] ![a] (Shared sds (Maybe a)) -> Task a | iTask a & RWShared sds
editSharedChoiceAs                   :: ![ChoiceOption o] ![o] !(o -> a) (Shared sds (Maybe a)) -> Task a | iTask o & iTask a & RWShared sds
editSharedMultipleChoice             :: ![ChoiceOption a] ![a] (Shared sds [a]) -> Task [a] | iTask a & RWShared sds
editSharedMultipleChoiceAs           :: ![ChoiceOption o] ![o] !(o -> a) (Shared sds [a]) -> Task [a] | iTask o & iTask a & RWShared sds

editSharedChoiceWithShared           :: ![ChoiceOption a] !(sds1 () [a] w) (Shared sds2 (Maybe a)) -> Task a | iTask a & TC w & RWShared sds1 & RWShared sds2
editSharedChoiceWithSharedAs         :: ![ChoiceOption o] !(sds1 () [o] w) (o -> a) (Shared sds2 (Maybe a)) -> Task a | iTask o & TC w & iTask a & RWShared sds1 & RWShared sds2
editSharedMultipleChoiceWithShared   :: ![ChoiceOption a] !(sds1 () [a] w) (Shared sds2 [a]) -> Task [a] | iTask a & TC w & RWShared sds1 & RWShared sds2
editSharedMultipleChoiceWithSharedAs :: ![ChoiceOption o] !(sds1 () [o] w) (o -> a) (Shared sds2 [a]) -> Task [a] | iTask o & TC w & iTask a & RWShared sds1 & RWShared sds2

/**
* Wait for a share to match a certain predicate
*
* @param Predicate:			A predicate to test when to continue. The task completes as soon as the predicate is true
* @param Shared:			Reference to the shared state to wait for
*
* @return					The value of the shared when the predicate becomes true
*/
wait :: (r -> Bool) !(sds () r w) -> Task r | iTask r & TC w & Registrable sds


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
viewSharedTitle :: !(sds () r w) -> Task r | iTask r & Registrable sds & TC w

/**
* Basic Create, Read, Update, Delete (CRUD) editor for a shared collection
*/
crud :: !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
        (sds () (f r) (f` w))
     -> Task r | iTask r & iTask (f r) & iTask w & iTask (f` w) & RWShared sds

crudWith :: ![ChoiceOption r] [EnterOption r] [ViewOption r] [UpdateOption r]
            !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
            (sds () (f r) (f` w))
         -> Task r | iTask r & iTask (f r) & iTask w & iTask (f` w) & RWShared sds
