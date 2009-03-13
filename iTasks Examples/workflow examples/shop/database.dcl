definition module database

import iTasks

derive gForm	DBRef
derive gUpd		DBRef
derive gPrint	DBRef
derive gParse	DBRef

class DB a where
	databaseId	:: DBid [a]
	getItemId	:: a -> DBRef a
	setItemId	:: (DBRef a) a -> a

:: DBRef a

instance == (DBRef a)
instance <  (DBRef a)
eqItemId 		:: a a -> Bool | DB a

dbReadAll		::                 Task [a]       | iData, DB a
dbWriteAll		:: ![a]         -> Task Void      | iData, DB a

//	C(reate)R(ead)U(pdate)D(elete) operations:
dbCreateItem	::                 Task a         | iData, DB a
dbReadItem		:: !(DBRef a)	-> Task (Maybe a) | iData, DB a
dbUpdateItem	:: a			-> Task a         | iData, DB a
dbDeleteItem	:: !(DBRef a)	-> Task Void      | iData, DB a
