definition module SharedTasks
/**
* This module provides tasks for dealing with shared references.
*/

import Task
from Shared import :: Shared, :: SymmetricShared

:: SharedStoreId :== String

/**
* Creates a reference to a store identified by a string identifier.
* Initially no data is stored and reading data will cause an exception.
*/
sharedStore :: !SharedStoreId -> SymmetricShared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

/**
* Creates a reference to a store identified by a string identifier.
* Initially the share contains a default value.
*/
sharedStoreDefault :: !SharedStoreId -> SymmetricShared a | JSONEncode{|*|}, JSONDecode{|*|}, gUpdate{|*|}, TC a

/**
* Create a shared store with automatically generated reference and given initial value.
* The store is automatically garbage collected after the process it was generated in terminates.
*
* @param An inital value
* @return A reference to the generated store
* @throws SharedException
*/
createSharedStore :: !a  -> Task (SymmetricShared a) | iTask a

/**
* Deletes a store with given identifier.
*
* @param A unique identifier
*/
deleteSharedStore :: !SharedStoreId -> Task Void

/**
* Reads shared data.
*
* @param A shared reference
* @return The value read
* @throws SharedException
*/
readShared :: !(Shared a w) -> Task a | iTask a

/**
* Writes shared data.
*
* @param A value to write
* @param A shared reference
* @return The value written
* @throws SharedException
*/
writeShared :: !(Shared r a) !a -> Task a | iTask a

/**
* Modifies shared data.
*
* @param A function modifying the shared value
* @param A shared reference
* @param The new value
* @throws SharedException
*/
updateShared :: !(r -> w) !(Shared r w) -> Task w | iTask r & iTask w

/**
* Puts a symmetric lens between two symmetric shared data sources.
* Changes of one also affects the other one.
*
* @param Shared a
* @param Shared b
* @param putr: used to map changes of shared a to shared b
* @param putl: used to map changes of shared b to shared a
* @param missing complement: includes all information not shared by a & b
* @param Shared references of the same type with symmetric lens between them
*/
symmetricLens :: !(aw c -> (bw,c)) !(bw c -> (aw,c)) !c !(Shared ar aw) !(Shared br bw) -> Task (!Shared ar aw,!Shared br bw) | iTask ar & iTask aw & iTask br & iTask bw & iTask c

//Convenience wrapper functions for databases with multiple values of type a 
class DB a where
	/*
	* Retrieve the database handle
	*/
	databaseId	:: SymmetricShared [a]
	/*
	* Retrieve the reference to a stored instance
	*/
	getItemId	:: a -> DBRef a
	/*
	* Set the a reference on a specific instance
	*/
	setItemId	:: (DBRef a) a -> a
	
//Database identifier to a value of type a in a database with multiple values
:: DBRef a = DBRef !Int

instance == (DBRef a)
instance <  (DBRef a)
derive class iTask DBRef

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
