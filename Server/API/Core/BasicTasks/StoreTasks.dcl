definition module StoreTasks
/**
* This module provides tasks for simple storage in the iTasks store
*/
from TSt 			import :: Task 
from StdOverloaded	import class ==, class <

from	iTasks import class iTask
import	GenVisualize, GenUpdate

//Database identifier for storing a single value of type a
::DBId a = DBId !String

instance == (DBId a)
instance toString (DBId a)

//Database identifier to a value of type a in a database with multiple values
:: DBRef a = DBRef !Int

instance == (DBRef a)
instance <  (DBRef a)

derive gVisualize	DBRef, DBId
derive gUpdate		DBRef, DBId
derive gVerify		DBRef, DBId
derive JSONEncode	DBRef, DBId
derive JSONDecode	DBRef, DBId

//Core database access functions

/**
* Create a database reference
*
* @param A unique name to identify the database with
* @return A database reference
*/
mkDBId 	:: !String -> (DBId a)
/**
* Create a database reference with automatically generated unique name
*
* @return A database reference
*/
createDBid :: Task (DBId a)
/**
* Create a database with automatically generated reference and given initial value
*
* @param Inital value
* @return A database reference
*/
createDB :: !a 				-> Task (DBId a) | iTask a
/**
* Read the database.
*
* @param The database reference
* @return The value in the database or a default value if no value is stored.
*/
readDB :: !(DBId a) 		-> Task a | iTask a
/**
* Read the database.
*
* @param The database reference
* @return The value in the database if a value is stored.
*/
readDBIfStored :: !(DBId a)	-> Task (Maybe a) | iTask a
/**
* Write the database.
*
* @param The database reference
* @param The new value to store in the database
* @return The new value of the database
*/
writeDB	:: !(DBId a) !a 	-> Task a | iTask a
/**
* Delete the database.
* 
* @param The database reference
* @param The value in the deleted database if stored.
*/
deleteDB :: !(DBId a)		-> Task (Maybe a) | iTask a
/**
* Modify the database.
*
* @param The database reference
* @param A function modifying the database
* @param The new value of the database
*/
modifyDB :: !(DBId a) (a -> a) -> Task a | iTask a

//Convenience wrapper functions for databases with multiple values of type a 
class DB a where
	/*
	* Retrieve the database handle
	*/
	databaseId	:: DBId [a]
	/*
	* Retrieve the reference to a stored instance
	*/
	getItemId	:: a -> DBRef a
	/*
	* Set the a reference on a specific instance
	*/
	setItemId	:: (DBRef a) a -> a

/*
* Checks whether two instances have equal database-references
*
* @param The first instance
* @param The second instance
*
* @return Whether both items have the same database-handle
*/
eqItemId 		:: a a -> Bool | DB a

/*
* Reads all instances from a database
*
* @return All instances in the database
*/
dbReadAll		::                 Task [a]       | iTask, DB a
/*
* Replaces an entire database with new data
*
* @param The instances which need to be stored
*
* @return The instances which were stored
*/
dbWriteAll		:: ![a]         -> Task [a]       | iTask, DB a

//	C(reate)R(ead)U(pdate)D(elete) operations:

/*
* Stores an item in the database and creates a new reference for this
* specific instance.
*
* @param The item which needs to be stored
*
* @return The stored instance
*/
dbCreateItem	:: a            -> Task a         | iTask, DB a

/*
* Retrieve an instance give a reference, if it exists
*
* @param The reference to the stored instance
*
* @return The stored instance if it exists
*/
dbReadItem		:: !(DBRef a)	-> Task (Maybe a) | iTask, DB a

/*
* Update the value of a specific instance in the database
*
* @param The new value
*
* @return The stored instance
*/
dbUpdateItem	:: a			-> Task a         | iTask, DB a

/*
* Delete an instance from the database
*
* @param The reference to the stored instance
*
* @return The removed instance, if it existed
*/
dbDeleteItem	:: !(DBRef a)	-> Task (Maybe a) | iTask, DB a
