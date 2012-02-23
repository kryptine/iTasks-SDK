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
from FilePath import :: FilePath

// Storage formats
:: StoreFormat = SFPlain | SFDynamic

:: StoreKey			:== String
:: StoreNamespace	:== String
:: StorePrefix		:== String

// Predefined namespaces
NS_SESSION_INSTANCES	:== "session-instances"
NS_PERSISTENT_INSTANCES	:== "persistent-instances"
NS_DOCUMENT_CONTENT		:== "document-data"
NS_APPLICATION_SHARES	:== "application-data"

/**
* Determine the location of the store from data directory and build
*/
storePath :: !FilePath !String -> FilePath

/**
* Store a value in the default format
*/
storeValue				:: !StoreNamespace !StoreKey !a				!*IWorld -> *IWorld							| JSONEncode{|*|}, TC a

/**
* Load a value from the store
*/
loadValue				:: !StoreNamespace !StoreKey				!*IWorld -> (!Maybe a,!*IWorld)				| JSONDecode{|*|}, TC a

/**
* Deletes the value with given key from the store
*/
deleteValue				:: !StoreNamespace !StoreKey				!*IWorld -> *IWorld

/**
* Get a value's version
*/
getStoreVersion			:: !StoreNamespace !StoreKey				!*IWorld -> (!Maybe Int,!*IWorld)

/**
* Load a value from the store, additionally a timestamp is given
*/
loadValueAndVersion		:: !StoreNamespace !StoreKey				!*IWorld -> (!Maybe (a,Int),!*IWorld)	| JSONDecode{|*|}, TC a

/**
* Deletes all values that start with the prefix from the store
*/
deleteValues			:: !StoreNamespace !StorePrefix					!*IWorld -> *IWorld
/**
* Determines if the store's value has been changed since given version
*/
isValueChanged			:: !StoreNamespace !StoreKey !Int !*IWorld -> (!Maybe Bool,!*IWorld)
