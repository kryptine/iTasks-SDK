definition module iTasksDB
/**
* Simple database tasks for local storage of data using iData.
*/
import TSt


DB_PREFIX 	:== "iDBase-"

derive gForm	DBRef
derive gUpd		DBRef
derive gPrint	DBRef
derive gParse	DBRef


//Core database access functions

//Database reference for storing a value of type a
:: DBid a

/**
* Create a database reference
*
* @param A unique name to identify the database with
* @param The storage type of the database. Either LSTxtFile or LSDataFile.
* @return A database reference
*/
mkDBid 	:: !String !Lifespan -> (DBid a)
/**
* Read the database.
*
* @param The database reference
* @return The value in the database
*/
readDB	:: !(DBid a) 		-> Task a | iData a
/**
* Write the database.
*
* @param The database reference
* @param The new value to store in the database
* @return The new value of the database
*/
writeDB	:: !(DBid a) !a 	-> Task a | iData a

//Convenience wrapper functions for databases with multiple values of type a 

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
