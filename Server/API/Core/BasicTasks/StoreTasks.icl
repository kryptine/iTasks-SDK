implementation module StoreTasks

import StdList, StdOrdList, StdTuple
import Shared, GenUpdate, GenVisualize, GenVerify, TSt
from StdFunc			import id
from CoreCombinators	import >>|, >>=, return

derive class iTask DBRef
derive bimap Maybe, (,)

instance == (DBRef a) where (==) (DBRef x) (DBRef y) = x == y
instance <  (DBRef a) where	(<)  (DBRef x) (DBRef y) = x <  y

// Core db access
createDBid :: Task (Shared a)
createDBid = mkInstantTask ("Create DB id", "Create a database identifier") createDBid`
where
	createDBid` tst=:{taskNr} = (TaskFinished (mkShared taskNr), tst)
	
createDB :: !a -> Task (Shared a) | iTask a
createDB init = mkInstantTask ("Create DB", "Create a database identifier & give initial value") createDB`
where
	createDB` tst=:{taskNr}
		# shared	= mkShared taskNr
		# tst		= appIWorldTSt (writeShared shared init) tst
		= (TaskFinished shared,tst)
	
mkShared taskNr = mkSharedReference ("DB_" +++ taskNrToString taskNr)

readDB :: !(shared a) -> Task a | toReadOnlyShared shared a & iTask a
readDB shared = mkInstantTask ("Read DB", "Read a value from the database") (accIWorldTSt readDB`)
where
	readDB` iworld
		# (mbVal,iworld) = readShared shared iworld
		= case mbVal of
			Just val
				= (TaskFinished val,iworld)
			Nothing		
				# (val,iworld) = defaultValue iworld
				= (TaskFinished val,iworld)
		
readDBIfStored :: !(shared a) -> Task (Maybe a) | toReadOnlyShared shared a & iTask a
readDBIfStored shared = mkInstantTask ("Read DB (conditional)", "Read a value from the database, if it exists") readDBIfStored`
where
	readDBIfStored` tst
		# (mbVal,tst) = accIWorldTSt (readShared shared) tst
		= (TaskFinished mbVal,tst)

writeDB	:: !(Shared a) !a -> Task a | iTask a
writeDB shared value = mkInstantTask ("Write DB", "Write a value to the database") writeDB`
where
	writeDB` tst
		# tst = appIWorldTSt (writeShared shared value) tst
		= (TaskFinished value,tst)
		
deleteDB :: !(Shared a) -> Task (Maybe a) | iTask a
deleteDB shared = mkInstantTask ("Delete DB", "Delete a value from the database") (accIWorldTSt deleteDB`)
where
	deleteDB` iworld
		# (mbVal,iworld)	= readShared shared iworld
		# iworld 			= deleteShared shared iworld
		= (TaskFinished mbVal,iworld)

updateDB :: !(Shared a) !(a -> a) -> Task a | iTask a
updateDB shared f = mkInstantTask ("Update DB", "Update the database") (accIWorldTSt updateDB`)
where
	updateDB` iworld
		# (mbVal,iworld) = readShared shared iworld
		# (val,iworld) = case mbVal of
			Just val	= (f val,iworld)
			Nothing		= app2 (f,id) (defaultValue iworld)
		# iworld = writeShared shared val iworld
		= (TaskFinished val,iworld)

//	Convenient operations on databases
eqItemId :: a a -> Bool | DB a
eqItemId a b	= getItemId a == getItemId b

dbReadAll :: Task [a] | iTask, DB a
dbReadAll		= readDB databaseId

dbWriteAll :: ![a] -> Task [a] | iTask, DB a
dbWriteAll all	= writeDB databaseId all

dbModify :: ([a] -> [a]) -> Task [a] | iTask, DB a
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

dbDeleteItem :: !(DBRef a) -> Task (Maybe a) | iTask, DB a
dbDeleteItem itemid
	= readDB databaseId >>= \items ->
		let (match, nomatch) = splitWith (\i -> getItemId i == itemid) items in
			dbWriteAll nomatch >>| case match of
				[] 			= return Nothing
				[item:_]	= return (Just item) 

//List utility functions
replace :: (a a -> Bool) a [a] -> [a]
replace cond new []         = [new]
replace cond new [x:xs]
    | cond new x            = [new : xs]
    | otherwise             = [x : replace cond new xs]

splitWith :: (a -> Bool) [a] -> ([a],[a])
splitWith f [] = ([],[])
splitWith f [x:xs]
	| f x	= let (y,n) = splitWith f xs in ([x:y],n)
			= let (y,n)	= splitWith f xs in (y,[x:n])