definition module Store
/**
* This module provides a simple generic store.
* It is used to store the internal databases with users, sessions and workflow processes
* and for storage of intermediate task results.
*
* Values can be stored either by generic serialization to plain text, or by writing a dynamic
* to disk.
* Dynamics are generally more expensive, so only when really necessary (for example to store tasks or
* functions) should they be used.
*/
import JSON
from Time import :: Timestamp
from IWorld import :: IWorld

// Storage formats
:: StoreFormat = SFPlain | SFDynamic

/**
* Store a value in the default format
*/
storeValue				:: !String !a				!*IWorld -> *IWorld							| JSONEncode{|*|}, TC a

/**
* Load a value from the store
*/
loadValue				:: !String					!*IWorld -> (!Maybe a,!*IWorld)				| JSONDecode{|*|}, TC a

/**
* Deletes the value with given key from the store
*/
deleteValue				:: !String					!*IWorld -> *IWorld

/**
* Get a value's timestamp
*/
getStoreTimestamp		:: !String					!*IWorld -> (!Maybe Timestamp,!*IWorld)

/**
* Load a value from the store, additionally a timestamp is given
*/
loadValueAndTimestamp	:: !String					!*IWorld -> (!Maybe (a,Timestamp),!*IWorld)	| JSONDecode{|*|}, TC a


/**
* Deletes all values that start with the prefix from the store
*/
deleteValues			:: !String					!*IWorld -> *IWorld
/** 
* Copy all values that start with the first prefix to a value prefixed with the second prefix
* E.g. copyValues "foo" "bar" changes "foo-23.txt" to "bar-23.txt"
*/
copyValues				:: !String !String !*IWorld -> *IWorld
/**
* Determines if the store's value has been changed since given timestamp
*/
isValueChanged			:: !String !Timestamp		!*IWorld -> (!Maybe Bool,!*IWorld)