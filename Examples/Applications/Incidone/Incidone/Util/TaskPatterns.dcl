definition module Incidone.Util.TaskPatterns

import iTasks
import Incidone.OP.Concepts, Incidone.Util.Workspace
import qualified Data.Map

//FIXME
//NoAnnotation	:== AfterLayout (tweakControls (map (\(c,_) -> (c,'Data.Map'.newMap))))
//FillNotes       :== AfterLayout (tweakControls fillNotes)

//fillNotes   :: [(UIControl,UIAttributes)] -> [(UIControl,UIAttributes)]

createNewIncident			:: Task (Maybe IncidentNo)
createNewContact			:: Task (Maybe ContactNo)

indexedStore        :: String v -> SDSLens k v v | Eq k & Ord k & iTask k & iTask v
sdsDeref            :: (sds1 p [a] [a]) (a -> Int) (sds2 [Int] [b] x) ([a] [b] -> [c]) -> (SDSSequence p [c] [a]) | iTask p & TC a & TC b & TC c & TC x & RWShared sds1 & RWShared sds2

// Information management
viewDetails	        :: (sds1 () (Maybe i) ()) (sds2 i c c) (c -> v) -> Task (Maybe v) | iTask i & iTask v & iTask c & RWShared sds1 & RWShared sds2

optionalNewOrOpen   :: (String,Task ()) (String,i -> Task ()) Workspace (sds () (Maybe i) ()) -> Task () | iTask i & RWShared sds

doAddRemoveOpen     :: (Task a) (r -> Task b) (r -> Task c) Workspace (sds () (Maybe r) w) -> Task () | iTask a & iTask b & iTask c & iTask r & RWShared sds & TC w

// Utility

viewAndEdit :: (Task a) (Task b) -> Task b | iTask a & iTask b
viewOrEdit :: (Shared sds a) (a a -> Task ()) -> Task () | iTask a & RWShared sds

doOrClose		:: (Task a)						-> Task (Maybe a) | iTask a
doOrCancel		:: (Task a)						-> Task (Maybe a) | iTask a

//Place a header
withHeader :: (Task a) (Task b) -> Task b | iTask a	& iTask b

viewNoSelection :: Task ()

//Task where a user has to explicitly choose between two tasks
oneOrAnother :: (String,Task a) (String,Task b) -> Task (Either a b) | iTask a & iTask b

//Allows you to enter a list with a task for each item and also enforce a minimum number of items
enterMultiple :: !String !Int (Task a) -> Task [a] | iTask a

//Work on multiple items from a shared list and add
manageSharedListWithDetails :: (Int -> Task ()) (Task Int) (Shared sds [Int]) -> Task () | RWShared sds

//Ok/Cancel transition
(>>?) infixl 1 :: !(Task a) !(a -> Task b) -> Task (Maybe b) | iTask a & iTask b

//Start/stop a background task
manageBackgroundTask :: !String !String (Task a) -> Task () | iTask a

//Reading network streams
syncNetworkChannel      :: String Int String (String -> m) (m -> String) (Shared sds ([m],Bool,[m],Bool)) -> Task () | iTask m & RWShared sds
consumeNetworkStream    :: ([m] -> Task ()) (Shared sds ([m],Bool,[m],Bool)) -> Task () | iTask m & RWShared sds

