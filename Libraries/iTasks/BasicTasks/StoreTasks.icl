implementation module StoreTasks

import TSt, Store
import StdList, StdOrdList, GenBimap
from StdFunc import id, const

import CoreCombinators
import GenPrint, GenParse, GenVisualize, GenUpdate

derive gVisualize	DBRef
derive gUpdate		DBRef
derive gPrint		DBRef
derive gParse		DBRef

::DBid a :== String

// Core db access

readDB :: !(DBid a) -> Task a | iTask a
readDB key = mkInstantTask "readDB" readDB`
where
	readDB` tst=:{store,world}
		# (mbVal,store,world) = loadValue (DB_PREFIX +++ key) store world
		= case mbVal of
			Just val	= (val,{tst & store = store, world = world})
			Nothing		= (defaultValue,{tst & store = store, world = world})

writeDB	:: !(DBid a) !a -> Task a | iTask a
writeDB key value = mkInstantTask "writeDB" writeDB`
where
	writeDB` tst=:{TSt|store}
		# store = storeValue (DB_PREFIX +++ key) value store
		= (value, {TSt| tst & store = store})
		
mkDBid :: !String -> (DBid a)
mkDBid s = s

//	Convenient operations on databases
:: DBRef a		= DBRef Int

instance == (DBRef a) where (==) (DBRef x) (DBRef y) = x == y
instance <  (DBRef a) where	(<)  (DBRef x) (DBRef y) = x <  y

eqItemId :: a a -> Bool | DB a
eqItemId a b	= getItemId a == getItemId b

dbReadAll :: Task [a] | iTask, DB a
dbReadAll		= readDB databaseId

dbWriteAll :: ![a] -> Task Void | iTask, DB a
dbWriteAll all	= writeDB databaseId all >>| return Void

dbModify :: ([a] -> [a]) -> Task Void | iTask, DB a
dbModify f      = dbReadAll >>= \items -> dbWriteAll (f items)

//	C(reate)R(ead)U(pdate)D(elete) operations:
dbCreateItem :: Task a | iTask, DB a
dbCreateItem
	= readDB databaseId >>= \items ->
	  let newid = newDBRef items 
	   in return (setItemId newid defaultValue)
where
	newDBRef :: [a] -> DBRef a | DB a
	newDBRef []		= DBRef 1
	newDBRef items	= let (DBRef i) = maxList (map getItemId items) in DBRef (i+1)

dbReadItem :: !(DBRef a) -> Task (Maybe a) | iTask, DB a
dbReadItem itemid
	= readDB databaseId >>= \items -> 
	  case filter (\item -> itemid == getItemId item) items of
	  	[found:_]	= return (Just found)
	  	nothing		= return Nothing

dbUpdateItem :: a -> Task a | iTask, DB a
dbUpdateItem new
	= dbModify (replace eqItemId new) >>| return new

dbDeleteItem :: !(DBRef a) -> Task Void | iTask, DB a
dbDeleteItem itemid
	= dbModify (filter (\item -> itemid <> getItemId item))
 
//List utility function
replace :: (a a -> Bool) a [a] -> [a]
replace cond new []         = [new]
replace cond new [x:xs]
    | cond new x            = [new : xs]
    | otherwise             = [x : replace cond new xs]

