definition module database

import iTasks

class DB a where
	databaseId	:: DBid [a]
	getItemId	:: a -> DBRef a
	setItemId	:: (DBRef a) a -> a

:: DBRef a		= DBRef Int

instance == (DBRef a)
instance <  (DBRef a)
eqItemId 		:: a a -> Bool | DB a

dbCreate		::  a           -> Task Void | iData, DB a
newDBRef		:: [a]          -> DBRef a   |        DB a
dbReadAll		::                 Task [a]  | iData, DB a
dbWriteAll		:: ![a]         -> Task Void | iData, DB a
dbModify		:: ([a] -> [a]) -> Task Void | iData, DB a
