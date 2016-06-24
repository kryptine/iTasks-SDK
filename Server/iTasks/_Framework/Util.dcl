definition module iTasks._Framework.Util

from StdClass import class Eq
from Data.Error import :: MaybeErrorString, :: MaybeError

import iTasks.API.Core.Types
from Data.Map import :: Map

mb2list				:: !(Maybe [a]) -> [a]
list2mb				:: ![a] -> (Maybe [a])

instance toString (Maybe a) | toString a

decFormat			:: !Int -> String

currentLocalDateTimeWorld	:: !*World	-> (!DateTime,!*World)
currentUTCDateTimeWorld	    :: !*World	-> (!DateTime,!*World)

timestampToGmDateTime	:: !Timestamp -> DateTime
dateToTimestamp			:: !Date -> Timestamp
datetimeToTimestamp 	:: !DateTime -> Timestamp

//Path conversion
toCanonicalPath			:: !FilePath !*World -> (!FilePath,!*World)

//Merging Maps
mergeMaps :: (Map k v) (Map k v) -> Map k v | < k

//Simple key value functions when fullblown maps are overkill
kvGet		:: k	![(k,v)]	-> Maybe v	| Eq k
kvSet		:: k v	![(k,v)]	-> [(k,v)]	| Eq k
kvSetOnce	:: k v	![(k,v)]	-> [(k,v)]	| Eq k
