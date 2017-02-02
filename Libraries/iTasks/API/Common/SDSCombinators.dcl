definition module iTasks.API.Common.SDSCombinators

from iTasks._Framework.SDS import :: RWShared, :: ROShared, :: WriteShare, :: SDSNotifyPred, :: Shared, :: ReadOnlyShared, :: ReadWriteShared
from iTasks._Framework.Task import :: TaskException, :: TaskValue, :: TaskId
from iTasks._Framework.Generic import class iTask, generic gEditor, generic gEq, generic gDefault, generic gText
from iTasks._Framework.Generic.Visualization import :: TextFormat
from iTasks.UI.Editor import :: Editor, :: EditMask, :: Masked
from iTasks.API.Core.Types import :: TaskList, :: TaskListFilter, :: TaskListItem, :: SharedTaskList, :: TaskAttributes, :: InstanceProgress

from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Error import :: MaybeError, :: MaybeErrorString
from Data.Map import :: Map
from Data.IntMap.Strict import :: IntMap
from StdOverloaded import class <

from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode

:: SDSReadProjection rs rt
    = SDSLensRead      (rs -> MaybeError TaskException rt) //Read lens-like
    | SDSConstRead     rt                                  //No need to read the original source

:: SDSWriteProjection rs ws wt
    = SDSLensWrite     (rs wt   -> MaybeError TaskException (Maybe ws)) //Write lens-like
    | SDSBlindWrite    (wt      -> MaybeError TaskException (Maybe ws)) //No-need to read the original source
    | SDSNoWrite

// Fix a focus parameter
sdsFocus     :: !p !(RWShared p r w) -> (RWShared p` r w) | iTask p

// Projection of the domain with a lens
sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(RWShared p rs ws) -> RWShared p r w | iTask p

// Translate the parameter space
sdsTranslate :: !String !(p -> ps) !(RWShared ps r w) -> RWShared p r w | iTask ps

// Introduce a new parameter
sdsSplit :: !String !(p -> (ps,pn)) !(pn rs -> r) !(pn rs w -> (ws,SDSNotifyPred pn)) !(RWShared ps rs ws) -> RWShared p r w | iTask ps & iTask pn

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapRead			:: !(r -> r`)					!(RWShared p r w) -> RWShared p r` w | iTask p
mapWrite		:: !(w` r -> Maybe w)			!(RWShared p r w) -> RWShared p r w` | iTask p
mapReadWrite	:: !(!r -> r`,!w` r -> Maybe w)	!(RWShared p r w) -> RWShared p r` w` | iTask p

mapReadError		:: !(r -> MaybeError TaskException r`)								!(RWShared p r w) -> RWShared p r` w | iTask p
mapWriteError		:: !(w` r -> MaybeError TaskException (Maybe w))					!(RWShared p r w) -> RWShared p r w` | iTask p
mapReadWriteError	:: !(!r -> MaybeError TaskException r`,!w` r -> MaybeError TaskException (Maybe w))	!(RWShared p r w) -> RWShared p r` w` | iTask p

toReadOnly :: !(RWShared p r w) -> ROShared p r | iTask p

toDynamic :: !(RWShared p r w) -> (RWShared p Dynamic Dynamic) | iTask p & TC r & TC w

//Map a list SDS of one element to the element itsel
mapSingle :: !(RWShared p [r] [w]) -> (RWShared p r w) | iTask p

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
// START DEPRECATED
(>+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) (wx,wy)     | iTask p
(>+|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wx          | iTask p
(|+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wy          | iTask p
(|+|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) ()          | iTask p
// END DEPRECATED
(>*<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) (wx,wy)     | iTask p
(>*|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wx          | iTask p
(|*<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wy          | iTask p
(|*|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) ()          | iTask p

/**
* Puts a symmetric lens between two symmetric shared data sources.
* Changes of one also affects the other one.
*
* @param putr: used to map changes of shared a to shared b
* @param putl: used to map changes of shared b to shared a
* @param SymmetricShared a
* @param SymmetricShared b
* @param RWShared references of the same type with symmetric lens between them
*/
symmetricLens :: !(a b -> b) !(b a -> a) !(RWShared p a a) !(RWShared p b b) -> (!RWShared p a a, !RWShared p b b) | iTask p

//Derived versions of tasks lists
/**
* Get the shared state of a task list
*/
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [TaskValue a]
/**
* Get the meta data sds of a task list
*/
taskListMeta	:: !(SharedTaskList a) -> ReadWriteShared [TaskListItem a] [(TaskId,TaskAttributes)]
/**
* Get the list of task id's in a task list
*/
taskListIds :: !(SharedTaskList a) -> ROShared () [TaskId]
/**
* Get the meta data sds for a specific entry in a task list
*/
taskListEntryMeta :: !(SharedTaskList a) -> RWShared TaskId (TaskListItem a) TaskAttributes
/*
* Get the id of the entry in the list the current task is part of
*/
taskListSelfId :: !(SharedTaskList a) -> ReadOnlyShared TaskId
/**
* Get the current tasks management meta data share
*/
taskListSelfManagement :: !(SharedTaskList a) -> Shared TaskAttributes
/**
* Get the value of a specific task in the list
* The paramater is either the index in the list or a specific task id
*/
taskListItemValue :: !(SharedTaskList a) -> ROShared (Either Int TaskId) (TaskValue a)
/**
* Get the progress of a specific task in the list
* The paramater is either the index in the list or a specific task id
*/
taskListItemProgress :: !(SharedTaskList a) -> ROShared (Either Int TaskId) InstanceProgress

/**
 * Convenience lens for lookups in Maps. Returns Nothing on a missing key.
 */
mapMaybeLens :: !String !(RWShared () (Map a b) (Map a b)) -> RWShared a (Maybe b) b | < a & == a

/**
 * Convenience lens for lookups in Maps. Can use a default value on a missing key, gives an error if no default is supplied.
 */
mapLens :: !String !(RWShared () (Map a b) (Map a b)) !(Maybe b) -> RWShared a b b | < a & == a

/**
 * Convenience lens for lookups in IntMaps. Can use a default value on a missing key, gives an error if no default is supplied.
 */
intMapLens :: !String !(RWShared () (IntMap a) (IntMap a)) !(Maybe a) -> RWShared Int a a