definition module iTasks.Internal.Util

from StdClass import class Eq
from Data.Error import :: MaybeErrorString, :: MaybeError
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from System.FilePath import :: FilePath
from iTasks.Extensions.DateTime import :: DateTime
from StdOverloaded import class <
from System.Time import :: Tm

show                :: ![String] !*World -> *World

mb2list				:: !(Maybe [a]) -> [a]
list2mb				:: ![a] -> (Maybe [a])

instance toString (Maybe a) | toString a

tmToDateTime :: !Tm -> DateTime

//Path conversion
toCanonicalPath			:: !FilePath !*World -> (!FilePath,!*World)

//Merging Maps
mergeMaps :: (Map k v) (Map k v) -> Map k v | < k

//Simple key value functions when fullblown maps are overkill
kvGet		:: k	![(k,v)]	-> Maybe v	| Eq k
kvSet		:: k v	![(k,v)]	-> [(k,v)]	| Eq k
kvSetOnce	:: k v	![(k,v)]	-> [(k,v)]	| Eq k

