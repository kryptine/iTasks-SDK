definition module StoreTasks
/**
* This module provides tasks for simple storage in the iTasks store
*/
from TSt 			import :: Task 
from StdOverloaded	import class ==, class <

from	iTasks import class iTask
import	GenPrint, GenParse, GenVisualize, GenUpdate

//Database identifier for storing a single value of type a
:: DBid a

//Database identifier to a value of type a in a database with multiple values
:: DBRef a = DBRef Int

derive gVisualize	DBRef
derive gUpdate		DBRef
derive gPrint		DBRef
derive gParse		DBRef

//Core database access functions

/**
* Create a database reference
*
* @param A unique name to identify the database with
* @param The storage type of the database. Either LSTxtFile or LSDataFile.
* @return A database reference
*/
mkDBid 	:: !String -> (DBid a)
/**
* Read the database.
*
* @param The database reference
* @return The value in the database
*/
readDB	:: !(DBid a) 		-> Task a | iTask a
/**
* Write the database.
*
* @param The database reference
* @param The new value to store in the database
* @return The new value of the database
*/
writeDB	:: !(DBid a) !a 	-> Task a | iTask a

//Convenience wrapper functions for databases with multiple values of type a 
class DB a where
	databaseId	:: DBid [a]
	getItemId	:: a -> DBRef a
	setItemId	:: (DBRef a) a -> a

instance == (DBRef a)
instance <  (DBRef a)

eqItemId 		:: a a -> Bool | DB a

dbReadAll		::                 Task [a]       | iTask, DB a
dbWriteAll		:: ![a]         -> Task Void      | iTask, DB a

//	C(reate)R(ead)U(pdate)D(elete) operations:
dbCreateItem	::                 Task a         | iTask, DB a
dbReadItem		:: !(DBRef a)	-> Task (Maybe a) | iTask, DB a
dbUpdateItem	:: a			-> Task a         | iTask, DB a
dbDeleteItem	:: !(DBRef a)	-> Task Void      | iTask, DB a
