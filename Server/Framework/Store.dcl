definition module Store
/**
* This module provides a simple generic store.
* It is used to store the internal databases with users, sessions and workflow processes
* and for storage of intermediate task results.
*
* The store has an in-memory cache which has to be flushed to disk after each http request.
* Values can be stored either by generic serialization to plain text, or by writing a dynamic
* to disk.
* Dynamics are generally more expensive, so only when really necessary (for example to store tasks or
* functions) should they be used.
*/
import JSON
from Time import :: Timestamp
from Types import :: IWorld

// Abstract store
:: *Store

// Storage format
:: StoreFormat = SFPlain | SFDynamic | SFBlob

/**
* Create a store
*/
createStore		:: !String -> *Store

/**
* Store a value in the default format
*/
storeValue				:: !String !a				!*IWorld -> *IWorld							| JSONEncode{|*|}, TC a

/**
* Store raw value
*/
storeValueAsBlob 		:: !String !String			!*IWorld -> *IWorld

/**
* Load a value from the store
*/
loadValue				:: !String					!*IWorld -> (!Maybe a,!*IWorld)				| JSONDecode{|*|}, TC a

/**
* Get a value's timestamp
*/
getStoreTimestamp		:: !String					!*IWorld -> (!Maybe Timestamp,!*IWorld)

/**
* Load a value from the store, additionally a timestamp is given
*/
loadValueAndTimestamp	:: !String					!*IWorld -> (!Maybe (a,Timestamp),!*IWorld)	| JSONDecode{|*|}, TC a

/**
* Load raw data from the store
*/
loadValueAsBlob 		:: !String					!*IWorld -> (!Maybe String,!*IWorld)

/**
* Deletes the value with given key from the store
*/
deleteValue				:: !String					!*IWorld -> *IWorld

/**
* Deletes all values that start with the prefix from the store
*/
deleteValues			:: !String					!*IWorld -> *IWorld

/**
* Copies all values in the store that start with the first prefix to
* a new name where the first prefix is replaced by the second.
*/
copyValues				:: !String !String			!*IWorld -> *IWorld

/**
* Writes all values stored in the cache to disk
*/
flushCache				::							!*IWorld -> *IWorld

/**
* Determines if the store's value has been changed since given timestamp
*/
isValueChanged			:: !String !Timestamp		!*IWorld -> (!Maybe Bool,!*IWorld)