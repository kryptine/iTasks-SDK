implementation module iTasksDB

import TSt
import StdList, StdOrdList, GenBimap
from StdFunc import id, const
import iDataTrivial, iDataFormlib
import LiftingCombinators, BasicCombinators

derive gForm	DBRef
derive gUpd		DBRef
derive gPrint	DBRef
derive gParse	DBRef

::DBid a :== (!String,!Lifespan)

// Core db access

readDB	:: !(DBid a) -> Task a | iData a
readDB 	name=:(idn,_) = appHSt ("readDB " +++ idn) (db name id)

writeDB	:: !(DBid a) !a -> Task a | iData a
writeDB name=:(idn,_) value = appHSt ("writeDB " +++ idn) (db name (const value))

db :: !(DBid a) !(a -> a) !*HSt -> (!a,!*HSt) | iData a
db (name,storageKind) fun hst 
	# (form,hst)	= mkStoreForm (Init,nFormId (DB_PREFIX +++ name) createDefault <@ storageKind <@ NoForm) fun hst
	= (form.Form.value,hst)

mkDBid :: !String !Lifespan -> (DBid a)
mkDBid s attr = (s,attr)

//	Convenient operations on databases
:: DBRef a		= DBRef Int

instance == (DBRef a) where (==) (DBRef x) (DBRef y) = x == y
instance <  (DBRef a) where	(<)  (DBRef x) (DBRef y) = x <  y

eqItemId :: a a -> Bool | DB a
eqItemId a b	= getItemId a == getItemId b

dbReadAll :: Task [a] | iData, DB a
dbReadAll		= readDB databaseId

dbWriteAll :: ![a] -> Task Void | iData, DB a
dbWriteAll all	= writeDB databaseId all #>> return_V Void

dbModify :: ([a] -> [a]) -> Task Void | iData, DB a
dbModify f      = dbReadAll =>> \items -> dbWriteAll (f items)

//	C(reate)R(ead)U(pdate)D(elete) operations:
dbCreateItem :: Task a | iData, DB a
dbCreateItem
	= readDB databaseId =>> \items ->
	  let newid = newDBRef items 
	   in return (setItemId newid createDefault)
where
	newDBRef :: [a] -> DBRef a | DB a
	newDBRef []		= DBRef 1
	newDBRef items	= let (DBRef i) = maxList (map getItemId items) in DBRef (i+1)

dbReadItem :: !(DBRef a) -> Task (Maybe a) | iData, DB a
dbReadItem itemid
	= readDB databaseId =>> \items -> 
	  case filter (\item -> itemid == getItemId item) items of
	  	[found:_]	= return (Just found)
	  	nothing		= return Nothing

dbUpdateItem :: a -> Task a | iData, DB a
dbUpdateItem new
	= dbModify (replace eqItemId new) #>> return new

dbDeleteItem :: !(DBRef a) -> Task Void | iData, DB a
dbDeleteItem itemid
	= dbModify (filter (\item -> itemid <> getItemId item))
 
//List utility function
replace :: (a a -> Bool) a [a] -> [a]
replace cond new []         = [new]
replace cond new [x:xs]
    | cond new x            = [new : xs]
    | otherwise             = [x : replace cond new xs]

