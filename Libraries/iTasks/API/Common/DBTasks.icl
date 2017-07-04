implementation module iTasks.API.Common.DBTasks

import StdList, StdOrdList, Data.List
import iTasks._Framework.Generic, iTasks._Framework.Task, iTasks._Framework.SDS
import iTasks._Framework.Util
import iTasks.API.Core.Types
import iTasks.API.Common.TaskCombinators
import iTasks.WF.Combinators.Overloaded

from iTasks.WF.Tasks.SDS import get, set

//	Convenient operations on databases
eqItemId :: a a -> Bool | DB a
eqItemId a b	= getItemId a == getItemId b

dbReadAll :: Task [a] | iTask, DB a
dbReadAll		= get databaseId

dbWriteAll :: ![a] -> Task [a] | iTask, DB a
dbWriteAll all	= set all databaseId 

dbModify :: ([a] -> [a]) -> Task [a] | iTask, DB a
dbModify f      = dbReadAll >>= \items -> dbWriteAll (f items)

//	C(reate)R(ead)U(pdate)D(elete) operations:
dbCreateItem :: a -> Task a | iTask, DB a
dbCreateItem new
	= get databaseId >>= \items -> 
	let newitem = (setItemId (newDBRef items) new) in
		dbWriteAll (items ++ [newitem]) >>| return newitem
where
	newDBRef :: [a] -> DBRef a | DB a
	newDBRef []		= DBRef 1
	newDBRef items	= let (DBRef i) = maxList (map getItemId items) in DBRef (i+1)

dbReadItem :: !(DBRef a) -> Task (Maybe a) | iTask, DB a
dbReadItem itemid
	= get databaseId >>= \items -> 
	  case filter (\item -> itemid == getItemId item) items of
	  	[found:_]	= return (Just found)
	  	nothing		= return Nothing

dbUpdateItem :: a -> Task a | iTask, DB a
dbUpdateItem new
	= dbModify (replaceInList eqItemId new) >>| return new

dbDeleteItem :: !(DBRef a) -> Task (Maybe a) | iTask, DB a
dbDeleteItem itemid
	= get databaseId >>= \items ->
		let (match, nomatch) = splitWith (\i -> getItemId i == itemid) items in
			dbWriteAll nomatch >>| case match of
				[] 			= return Nothing
				[item:_]	= return (Just item)
				
derive class iTask DBRef

instance == (DBRef a) where (==) (DBRef x) (DBRef y) = x == y
instance <  (DBRef a) where	(<)  (DBRef x) (DBRef y) = x <  y
