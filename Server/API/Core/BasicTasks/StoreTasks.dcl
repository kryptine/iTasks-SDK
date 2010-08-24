definition module StoreTasks
/**
* This module provides tasks for simple storage in the iTasks store
*/
from TSt 			import :: Task 
from StdOverloaded	import class ==, class <

from	iTasks import class iTask
import	GenVisualize, GenUpdate, GenMerge

//Database identifier for storing a single value of type a
::DBid a :== String

//Database identifier to a value of type a in a database with multiple values
:: DBRef a = DBRef Int

derive gVisualize		DBRef
derive gUpdate			DBRef
derive gVerify			DBRef
derive gMerge			DBRef

derive JSONEncode		DBRef
derive JSONDecode		DBRef

//Core database access functions

/**
* Create a database reference
*
* @param A unique name to identify the database with
* @return A database reference
*/
mkDBid 	:: !String -> (DBid a)
/**
* Create a database reference with automatically generated unique name
*
* @return A database reference
*/
createDBid :: Task (DBid a)
/**
* Create a database with automatically generated reference and given initial value
*
* @param Inital value
* @return A database reference
*/
createDB :: !a 				-> Task (DBid a) | iTask a
/**
* Read the database.
*
* @param The database reference
* @return The value in the database or a default value if no value is stored.
*/
readDB :: !(DBid a) 		-> Task a | iTask a
/**
* Read the database.
*
* @param The database reference
* @return The value in the database if a value is stored.
*/
readDBIfStored :: !(DBid a)	-> Task (Maybe a) | iTask a
/**
* Write the database.
*
* @param The database reference
* @param The new value to store in the database
* @return The new value of the database
*/
writeDB	:: !(DBid a) !a 	-> Task a | iTask a
/**
* Delete the database.
*
* @param The database reference
*/
deleteDB :: !(DBid a)		-> Task Void
/**
* Modify the database.
*
* @param The database reference
* @param A function modifying the database
* @param The new value of the database
*/
modifyDB :: !(DBid a) (a -> a) -> Task a | iTask a

//Convenience wrapper functions for databases with multiple values of type a 
class DB a where
	databaseId	:: DBid [a]
	getItemId	:: a -> DBRef a
	setItemId	:: (DBRef a) a -> a

instance == (DBRef a)
instance <  (DBRef a)

eqItemId 		:: a a -> Bool | DB a

dbReadAll		::                 Task [a]       | iTask, DB a
dbWriteAll		:: ![a]         -> Task [a]       | iTask, DB a

//	C(reate)R(ead)U(pdate)D(elete) operations:
dbCreateItem	:: a            -> Task a         | iTask, DB a
dbReadItem		:: !(DBRef a)	-> Task (Maybe a) | iTask, DB a
dbUpdateItem	:: a			-> Task a         | iTask, DB a
dbDeleteItem	:: !(DBRef a)	-> Task (Maybe a) | iTask, DB a
