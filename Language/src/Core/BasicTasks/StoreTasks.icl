implementation module StoreTasks

import TSt, Store
import StdList, StdOrdList
from StdFunc import id, const

from SystemTasks import getDefaultValue
import CoreCombinators
import GenPrint, GenParse, GenVisualize, GenUpdate

derive gVisualize	DBRef
derive gUpdate		DBRef
derive gPrint		DBRef
derive gParse		DBRef

derive bimap	Maybe, (,)

::DBid a :== String

// Core db access

readDB :: !(DBid a) -> Task a | iTask a
readDB key = mkInstantTask "readDB" readDB`
where
	readDB` tst=:{TSt|dataStore,world}
		# (mbVal,dstore,world) = loadValue key dataStore world
		= case mbVal of
			Just val
				= (TaskFinished val,{TSt|tst & dataStore = dstore, world = world})
			Nothing		
				# (val,world) = defaultValue world
				= (TaskFinished val,{TSt|tst & dataStore = dstore, world = world})

writeDB	:: !(DBid a) !a -> Task a | iTask a
writeDB key value = mkInstantTask "writeDB" writeDB`
where
	writeDB` tst=:{TSt|dataStore}
		# dstore = storeValue key value dataStore
		= (TaskFinished value, {TSt| tst & dataStore = dstore})
		
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
dbCreateItem :: a -> Task a | iTask, DB a
dbCreateItem new
	= readDB databaseId >>= \items -> 
	let newitem = (setItemId (newDBRef items) new) in
		dbWriteAll (items ++ [newitem]) >>| return newitem
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

