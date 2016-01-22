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

indexedStore        :: String v -> RWShared k v v | Eq k & Ord k & iTask k & iTask v
sdsDeref            :: (RWShared p [a] [a]) (a -> Int) (RWShared [Int] [b] x) ([a] [b] -> [c]) -> (RWShared p [c] [a])

// Information management
viewDetails	        :: !d (ReadOnlyShared (Maybe i)) (RWShared i c c) (c -> v) -> Task (Maybe v) | toPrompt d & iTask i & iTask v

optionalNewOrOpen   :: (String,Task ()) (String,i -> Task ()) Workspace (ReadOnlyShared (Maybe i)) -> Task () | iTask i

doAddRemoveOpen		:: (Task a) (r -> Task b) (r -> Task c) Workspace (ReadWriteShared (Maybe r) w) -> Task () | iTask a & iTask b & iTask c & iTask r

// Utility

viewAndEdit :: (Task a) (Task b) -> Task b | iTask a & iTask b
viewOrEdit :: d (Shared a) (a a -> Task ()) -> Task () | toPrompt d & iTask a

doOrClose		:: (Task a)						-> Task (Maybe a) | iTask a
doOrCancel		:: (Task a)						-> Task (Maybe a) | iTask a

//Place a header
withHeader :: (Task a) (Task b) -> Task b | iTask a	& iTask b

viewNoSelection :: Task ()

//Task where a user has to explicitly choose between two tasks
oneOrAnother :: !d (String,Task a) (String,Task b) -> Task (Either a b) | toPrompt d & iTask a & iTask b

//Allows you to enter a list with a task for each item and also enforce a minimum number of items
enterMultiple :: !String !Int (Task a) -> Task [a] | iTask a

//Work on multiple items from a shared list and add
manageSharedListWithDetails :: (Int -> Task ()) (Task Int) (Shared [Int]) -> Task ()

//Ok/Cancel transition
(>>?) infixl 1 :: !(Task a) !(a -> Task b) -> Task (Maybe b) | iTask a & iTask b


//Start/stop a background task
manageBackgroundTask :: !d !String !String (Task a) -> Task () | toPrompt d & iTask a

//Reading network streams
syncNetworkChannel      :: String Int String (String -> m) (m -> String) (Shared ([m],Bool,[m],Bool)) -> Task () | iTask m
consumeNetworkStream    :: ([m] -> Task ()) (Shared ([m],Bool,[m],Bool)) -> Task () | iTask m

