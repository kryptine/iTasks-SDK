implementation module database

import StdList, StdOrdList
import iTasks

//	Operations on database

// Polymorhpic database operations
class DB a where
	databaseId	:: (DBid [a])
	getItemId	:: a -> DBRef a
	setItemId	:: (DBRef a) a -> a

:: DBRef a		= DBRef Int
	
// CRUD
dbCreate :: a -> Task Void | iData, DB a
dbCreate item	= writeDB databaseId [setItemId (DBRef 0) item]	#>> return_V Void

newDBRef :: [a] -> DBRef a | DB a
newDBRef items	= let (DBRef i) = maxList (map getItemId items) in DBRef (i+1)
	
dbReadAll :: Task [a] | iData, DB a
dbReadAll		= readDB databaseId

dbWriteAll :: [a] -> Task Void | iData, DB a
dbWriteAll all	= writeDB databaseId all #>> return_V Void

dbModify :: ([a] -> [a]) -> Task Void | iData, DB a
dbModify f		= dbReadAll =>> \items -> dbWriteAll (f items)

instance == (DBRef a) where (==) (DBRef x) (DBRef y) = x == y
instance <  (DBRef a) where	(<)  (DBRef x) (DBRef y) = x <  y

eqItemId :: a a -> Bool | DB a
eqItemId a b	= getItemId a == getItemId b
