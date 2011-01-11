implementation module StoreTasks

import TSt, Store
import StdList, StdOrdList
from StdFunc import id, const

from SystemTasks import getDefaultValue
import CoreCombinators
import GenVisualize, GenUpdate
from InteractionTasks import instance html String

derive gVisualize		DBRef, DBId
derive gUpdate			DBRef, DBId
derive gVerify			DBRef, DBId
derive gMerge			DBRef, DBId

derive JSONEncode		DBRef, DBId
derive JSONDecode		DBRef, DBId

derive bimap Maybe, (,)

instance == (DBId a) where (==) (DBId x) (DBId y) = x == y
instance toString (DBId a) where toString (DBId x) = x

instance == (DBRef a) where (==) (DBRef x) (DBRef y) = x == y
instance <  (DBRef a) where	(<)  (DBRef x) (DBRef y) = x <  y

// Core db access
createDBid :: Task (DBId a)
createDBid = mkInstantTask ("Create DB id", "Create a database identifier") createDBid`
where
	createDBid` tst=:{taskNr} = (TaskFinished (mkDBId ("DB_" +++ taskNrToString taskNr)), tst)
	
createDB :: !a -> Task (DBId a) | iTask a
createDB init =
				createDBid
	>>= \id.	writeDB id init
	>>|			return id

readDB :: !(DBId a) -> Task a | iTask a
readDB (DBId key) = mkInstantTask ("Read DB", "Read a value from the database") readDB`
where
	readDB` tst
		# (mbVal,tst) = accIWorldTSt (loadValue key) tst
		= case mbVal of
			Just val
				= (TaskFinished val,tst)
			Nothing		
				# (val,tst) = accIWorldTSt defaultValue tst
				= (TaskFinished val,tst)
		
readDBIfStored :: !(DBId a)	-> Task (Maybe a) | iTask a
readDBIfStored (DBId key) = mkInstantTask ("Read DB (conditional)", "Read a value from the database, if it exists") readDBIfStored`
where
	readDBIfStored` tst
		# (mbVal,tst) = accIWorldTSt (loadValue key) tst
		= (TaskFinished mbVal,tst)

writeDB	:: !(DBId a) !a -> Task a | iTask a
writeDB (DBId key) value = mkInstantTask ("Write DB", "Write a value to the database") writeDB`
where
	writeDB` tst
		# tst = appIWorldTSt (storeValue key value) tst
		= (TaskFinished value,tst)
		
deleteDB :: !(DBId a) -> Task (Maybe a) | iTask a
deleteDB (DBId key) = mkInstantTask ("Delete DB", "Delete a value from the database") deleteDB`
where
	deleteDB` tst
		# (mbVal,tst)	= accIWorldTSt (loadValue key) tst
		# tst 			= appIWorldTSt (deleteValues key) tst
		= (TaskFinished mbVal,tst)

modifyDB :: !(DBId a) (a -> a) -> Task a | iTask a
modifyDB key f =
			readDB key
	>>= \v.	writeDB key (f v)
		
mkDBId :: !String -> (DBId a)
mkDBId s = DBId s

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

