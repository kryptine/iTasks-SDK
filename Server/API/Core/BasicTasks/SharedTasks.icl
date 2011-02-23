implementation module SharedTasks

import StdList, StdOrdList, StdTuple, Error, Util
import GenUpdate, GenVisualize, GenVerify, TSt, ExceptionCombinators
from StdFunc			import id, o
from CoreCombinators	import >>|, >>=, return
from Shared				import :: Shared(..), :: SharedWrite, :: SharedRead, :: SharedGetTimestamp, :: SymmetricShared, mapShared, >+<
from Shared				import qualified writeShared, readShared

sharedStore :: !SharedStoreId -> SymmetricShared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId = Shared
	(get loadValue ("cannot load value from store '" +++ storeId +++ "'"))
	write
	(get getStoreTimestamp ("cannot get timestamp of store '" +++ storeId +++ "'"))
where	
	get f err iworld
		# (mbV,iworld) = f storeId iworld
		# res = case mbV of
			Nothing	= Error err
			Just v	= Ok v
		= (res,iworld)
		
	write v iworld = (Ok Void,storeValue storeId v iworld)

sharedStoreDefault :: !SharedStoreId -> SymmetricShared a | JSONEncode{|*|}, JSONDecode{|*|}, gUpdate{|*|}, TC a
sharedStoreDefault storeId = Shared read write timestamp
where
	read iworld
		# (mbV,iworld) = loadValue storeId iworld
		= case mbV of
			Nothing
				# (v,iworld)	= defaultValue iworld
				# iworld = storeValue storeId v iworld
				= (Ok v,iworld)
			Just v
				= (Ok v,iworld)	
	write v iworld
		= (Ok Void,storeValue storeId v iworld)
	timestamp iworld
		# (mbV,iworld) = getStoreTimestamp storeId iworld
		= case mbV of
			Nothing	= (Error ("cannot get timestamp of store '" +++ storeId +++ "'"), iworld)
			Just v	= (Ok v, iworld)
	
createSharedStore :: !a  -> Task (SymmetricShared a) | iTask a
createSharedStore init
	= mkInstantTask ("Create shared store", "Creates a shared store") createSharedStore`
where
	createSharedStore` tst=:{taskNr,properties=p=:{systemProperties=s=:{SystemProperties|taskId}}}
		// store name starts with 'iTask_{id of main task}' to include it in garbage collection if process finishes
		// in the rest of the name the id of the current task serves as unique id for the store
		# shared		= sharedStore ("iTask_" +++ taskId +++ "-" +++ taskNrToString taskNr +++ "-shared")
		# (wres,tst)	= accIWorldTSt ('Shared'.writeShared shared init) tst
		= case wres of
			Ok _	= (TaskFinished shared,tst)
			Error e	= (TaskException (dynamic (SharedException e)),tst)
			
deleteSharedStore :: !SharedStoreId -> Task Void
deleteSharedStore id
	= mkInstantTask ("Delete shared store","Deletes a shared store with given identifier") deleteSharedStore`
where
	deleteSharedStore` tst
		# tst = appIWorldTSt (deleteValue id) tst
		= (TaskFinished Void,tst)

readShared :: !(Shared a w) -> Task a | iTask a
readShared shared
	= mkInstantTask ("Read shared", "Reads a shared value") (accIWorldTSt readShared`)
where
	readShared` iworld
		# (val,iworld) = 'Shared'.readShared shared iworld
		# res = case val of
			Ok val	= TaskFinished val
			Error e	= TaskException (dynamic (SharedException e))
		= (res,iworld)
		
writeShared :: !(Shared r a) !a -> Task a | iTask a
writeShared shared val
	= mkInstantTask ("Write shared", "Writes a shared value") (accIWorldTSt writeShared`)
where
	writeShared` iworld
		# (res,iworld) = 'Shared'.writeShared shared val iworld
		# res = case res of
			Ok _	= TaskFinished val
			Error e	= TaskException (dynamic (SharedException e))
		= (res,iworld)

updateShared :: !(Shared r w) !(r -> w) -> Task w | iTask r & iTask w
updateShared shared f
	= mkInstantTask ("Update shared", "Updates a shared value") (accIWorldTSt updateShared`)
where
	updateShared` iworld
		# (val,iworld)	= 'Shared'.readShared shared iworld
		| isError val	= (TaskException (dynamic (SharedException (fromError val))),iworld)
		# val			= f (fromOk val)
		# (wres,iworld)	= 'Shared'.writeShared shared val iworld
		| isError wres	= (TaskException (dynamic (SharedException (fromError wres))),iworld)
		= (TaskFinished val,iworld)

symmetricLens :: !(SymmetricShared a) !(SymmetricShared b) !(a c -> (b,c)) !(b c -> (a,c)) !c -> Task (!SymmetricShared a,!SymmetricShared b) | iTask a & iTask b & iTask c
symmetricLens sharedA sharedB putr putl missing =
					createSharedStore missing
	>>=	\sharedC.	return (sharedA >+< sharedB >+< sharedC)
	>>= \sharedAll.	return (newSharedA sharedAll,newSharedB sharedAll)
where
	newSharedA sharedAll = mapShared (fst o fst,\a ((_,b),c) -> let (b`,c`) = putr a c in ((a,b`),c`)) sharedAll
	newSharedB sharedAll = mapShared (snd o fst,\b ((a,_),c) -> let (a`,c`) = putl b c in ((a`,b),c`)) sharedAll
	
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
