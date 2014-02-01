definition module iTasks.Framework.Util

from StdClass import class Eq
from Data.Error import :: MaybeErrorString, :: MaybeError

import iTasks.API.Core.Types

mb2list				:: !(Maybe [a]) -> [a]
list2mb				:: ![a] -> (Maybe [a])

voidNothing 		:: Maybe Void

camelCaseToWords 	:: !String -> String

instance toString (Maybe a) | toString a

decFormat			:: !Int -> String

// Functions for accessing dates and times
currentTimestamp		:: !*IWorld -> (!Timestamp,!*IWorld)
currentTimestampError	:: !*IWorld -> (!MaybeErrorString Timestamp,!*IWorld)

currentLocalDateTimeWorld	:: !*World	-> (!DateTime,!*World)
currentUTCDateTimeWorld	    :: !*World	-> (!DateTime,!*World)

timestampToGmDateTime	:: !Timestamp -> DateTime
dateToTimestamp			:: !Date -> Timestamp

//Path conversion
toCanonicalPath			:: !FilePath !*World -> (!FilePath,!*World)

//Simple key value functions when fullblown maps are overkill
kvGet		:: k	![(k,v)]	-> Maybe v	| Eq k
kvSet		:: k v	![(k,v)]	-> [(k,v)]	| Eq k
kvSetOnce	:: k v	![(k,v)]	-> [(k,v)]	| Eq k
