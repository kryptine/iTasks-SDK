implementation module SharedTasks

import StdList, StdOrdList, StdTuple, Error, Util
import GenUpdate, GenVisualize, GenVerify, TSt, ExceptionCombinators
from StdFunc			import id, o, const
from CoreCombinators	import >>|, >>=
from CoreTasks			import return
from Shared				import :: Shared(..), :: SharedWrite, :: SharedRead, :: SharedGetTimestamp, :: SymmetricShared
from Shared				import qualified writeShared, readShared

sharedStore :: !SharedStoreId !a -> SymmetricShared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = Shared
	(get loadValue defaultV)
	write
	(get getStoreTimestamp (Timestamp 0))
where	
	get f defaultV iworld
		# (mbV,iworld) = f storeId iworld
		# res = case mbV of
			Nothing	= Ok defaultV
			Just v	= Ok v
		= (res,iworld)
		
	write v iworld = (Ok Void,storeValue storeId v iworld)
	
createSharedStore :: !a  -> Task (SymmetricShared a) | iTask a
createSharedStore init
	= mkInstantTask ("Create shared store", "Creates a shared store") createSharedStore`
where
	createSharedStore` tst=:{taskNr,properties=p=:{systemProperties=s=:{SystemProperties|taskId}}}
		// store name starts with 'iTask_{id of main task}' to include it in garbage collection if process finishes
		// in the rest of the name the id of the current task serves as unique id for the store
		# shared		= sharedStore (iTaskId taskId (taskNrToString taskNr +++ "-shared")) init
		= (TaskFinished shared,tst)
			
deleteSharedStore :: !SharedStoreId -> Task Void
deleteSharedStore id
	= mkInstantTask ("Delete shared store","Deletes a shared store with given identifier") deleteSharedStore`
where
	deleteSharedStore` tst
		# tst = appIWorldTSt (deleteValue id) tst
		= (TaskFinished Void,{tst & sharedDeleted = True})

readShared :: !(Shared a w) -> Task a | iTask a
readShared shared
	= mkInstantTask ("Read shared", "Reads a shared value") (accIWorldTSt readShared`)
where
	readShared` iworld
		# (val,iworld) = 'Shared'.readShared shared iworld
		# res = case val of
			Ok val	= TaskFinished val
			Error e	= taskException (SharedException e)
		= (res,iworld)
	
writeShared :: !(Shared r a) !a -> Task a | iTask a
writeShared shared val
	= mkInstantTask ("Write shared", "Writes a shared value") writeShared`
where
	writeShared` tst
		// set shared changed flag
		# tst		= {tst & sharedChanged = True}
		# (res,tst)	= accIWorldTSt ('Shared'.writeShared shared val) tst
		# res = case res of
			Ok _	= TaskFinished val
			Error e	= taskException (SharedException e)
		= (res,tst)

updateShared :: !(r -> w) !(Shared r w) -> Task w | iTask r & iTask w
updateShared f shared
	= mkInstantTask ("Update shared", "Updates a shared value") (\tst -> accIWorldTSt updateShared` {tst & sharedChanged = True})
where
	updateShared` iworld
		# (val,iworld)	= 'Shared'.readShared shared iworld
		| isError val	= (taskException (SharedException (fromError val)),iworld)
		# val			= f (fromOk val)
		# (wres,iworld)	= 'Shared'.writeShared shared val iworld
		| isError wres	= (taskException (SharedException (fromError wres)),iworld)
		= (TaskFinished val,iworld)
	
//	Convenient operations on databases
eqItemId :: a a -> Bool | DB a
eqItemId a b	= getItemId a == getItemId b

dbReadAll :: Task [a] | iTask, DB a
dbReadAll		= readShared databaseId

dbWriteAll :: ![a] -> Task [a] | iTask, DB a
dbWriteAll all	= writeShared databaseId all

dbModify :: ([a] -> [a]) -> Task [a] | iTask, DB a
dbModify f      = dbReadAll >>= \items -> dbWriteAll (f items)

//	C(reate)R(ead)U(pdate)D(elete) operations:
dbCreateItem :: a -> Task a | iTask, DB a
dbCreateItem new
	= readShared databaseId >>= \items -> 
	let newitem = (setItemId (newDBRef items) new) in
		dbWriteAll (items ++ [newitem]) >>| return newitem
where
	newDBRef :: [a] -> DBRef a | DB a
	newDBRef []		= DBRef 1
	newDBRef items	= let (DBRef i) = maxList (map getItemId items) in DBRef (i+1)

dbReadItem :: !(DBRef a) -> Task (Maybe a) | iTask, DB a
dbReadItem itemid
	= readShared databaseId >>= \items -> 
	  case filter (\item -> itemid == getItemId item) items of
	  	[found:_]	= return (Just found)
	  	nothing		= return Nothing

dbUpdateItem :: a -> Task a | iTask, DB a
dbUpdateItem new
	= dbModify (replaceInList eqItemId new) >>| return new

dbDeleteItem :: !(DBRef a) -> Task (Maybe a) | iTask, DB a
dbDeleteItem itemid
	= readShared databaseId >>= \items ->
		let (match, nomatch) = splitWith (\i -> getItemId i == itemid) items in
			dbWriteAll nomatch >>| case match of
				[] 			= return Nothing
				[item:_]	= return (Just item)
				
derive class iTask DBRef
derive bimap Maybe, (,)

instance == (DBRef a) where (==) (DBRef x) (DBRef y) = x == y
instance <  (DBRef a) where	(<)  (DBRef x) (DBRef y) = x <  y
