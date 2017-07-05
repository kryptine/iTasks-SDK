definition module iTasks._Framework.Util

from StdClass import class Eq
from Data.Error import :: MaybeErrorString, :: MaybeError
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from System.FilePath import :: FilePath
from iTasks.API.Extensions.DateTime import :: DateTime
from StdOverloaded import class <

show                :: ![String] !*World -> *World

mb2list				:: !(Maybe [a]) -> [a]
list2mb				:: ![a] -> (Maybe [a])

instance toString (Maybe a) | toString a

currentLocalDateTimeWorld	:: !*World	-> (!DateTime,!*World)
currentUTCDateTimeWorld	    :: !*World	-> (!DateTime,!*World)

//Path conversion
toCanonicalPath			:: !FilePath !*World -> (!FilePath,!*World)

//Merging Maps
mergeMaps :: (Map k v) (Map k v) -> Map k v | < k

//Simple key value functions when fullblown maps are overkill
kvGet		:: k	![(k,v)]	-> Maybe v	| Eq k
kvSet		:: k v	![(k,v)]	-> [(k,v)]	| Eq k
kvSetOnce	:: k v	![(k,v)]	-> [(k,v)]	| Eq k

