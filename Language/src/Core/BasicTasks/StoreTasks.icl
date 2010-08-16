implementation module StoreTasks

import TSt, Store
import StdList, StdOrdList
from StdFunc import id, const

from SystemTasks import getDefaultValue
import CoreCombinators
import GenVisualize, GenUpdate

derive gVisualize		DBRef
derive gUpdate			DBRef
derive gHint			DBRef
derive gError			DBRef
derive gMerge			DBRef

derive JSONEncode		DBRef
derive JSONDecode		DBRef

derive bimap Maybe, (,)

::DBid a :== String

// Core db access
createDBid :: Task (DBid a)
createDBid = mkInstantTask "Create DB id" "Create a database identifier" createDBid`
where
	createDBid` tst=:{taskNr} = (TaskFinished (mkDBid "DB_" +++ taskNrToString taskNr), tst)
	
createDB :: !a -> Task (DBid a) | iTask a
createDB init =
				createDBid
	>>= \id.	writeDB id init
	>>|			return id

readDB :: !(DBid a) -> Task a | iTask a
readDB key = mkInstantTask "Read DB" "Read a value from the database" readDB`
where
	readDB` tst=:{TSt|iworld=iworld=:{IWorld|store,world}}
		# (mbVal,store,world) = loadValue key store world
		= case mbVal of
			Just val
				= (TaskFinished val,{TSt|tst & iworld = {IWorld|iworld & store = store, world = world}})
			Nothing		
				# (val,iworld)	= defaultValue {IWorld|iworld & store = store, world = world}
				= (TaskFinished val,{TSt|tst & iworld = iworld})
		
readDBIfStored :: !(DBid a)	-> Task (Maybe a) | iTask a
readDBIfStored key = mkInstantTask "Read DB (conditional)" "Read a value from the database, if it exists" readDBIfStored`
where
	readDBIfStored` tst=:{TSt|iworld=iworld=:{IWorld|store,world}}
		# (mbVal,store,world)	= loadValue key store world
		= (TaskFinished mbVal,{TSt|tst & iworld = {IWorld|iworld & store = store, world = world}})

writeDB	:: !(DBid a) !a -> Task a | iTask a
writeDB key value = mkInstantTask "Write DB" "Write a value to the database" writeDB`
where
	writeDB` tst=:{TSt|iworld=iworld=:{IWorld|store}}
		# store = storeValue key value store
		= (TaskFinished value, {TSt| tst & iworld = {IWorld|iworld & store = store}})
		
deleteDB :: !(DBid a) -> Task Void
deleteDB key = mkInstantTask "Delete DB" "Delete a value from the database" deleteDB`
where
	deleteDB` tst=:{TSt|iworld=iworld=:{IWorld|store,world}}
		# (store,world) = deleteValues key store world
		= (TaskFinished Void, {TSt|tst & iworld = {IWorld|iworld & store = store, world = world}})

modifyDB :: !(DBid a) (a -> a) -> Task a | iTask a
modifyDB key f =
			readDB key
	>>= \v.	writeDB key (f v)
		
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

