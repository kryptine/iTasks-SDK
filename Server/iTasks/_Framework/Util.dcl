definition module iTasks._Framework.Util

from StdClass import class Eq
from Data.Error import :: MaybeErrorString, :: MaybeError

import iTasks.API.Core.Types

mb2list				:: !(Maybe [a]) -> [a]
list2mb				:: ![a] -> (Maybe [a])

voidNothing 		:: Maybe Void

instance toString (Maybe a) | toString a

decFormat			:: !Int -> String

currentLocalDateTimeWorld	:: !*World	-> (!DateTime,!*World)
currentUTCDateTimeWorld	    :: !*World	-> (!DateTime,!*World)

timestampToGmDateTime	:: !Timestamp -> DateTime
dateToTimestamp			:: !Date -> Timestamp
datetimeToTimestamp 	:: !DateTime -> Timestamp

//Path conversion
toCanonicalPath			:: !FilePath !*World -> (!FilePath,!*World)

//Simple key value functions when fullblown maps are overkill
kvGet		:: k	![(k,v)]	-> Maybe v	| Eq k
kvSet		:: k v	![(k,v)]	-> [(k,v)]	| Eq k
kvSetOnce	:: k v	![(k,v)]	-> [(k,v)]	| Eq k
