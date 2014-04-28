definition module iTasks.Framework.Store
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
//import Text.JSON
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.Maybe import :: Maybe
from Data.Void import :: Void
from System.Time import :: Timestamp
from System.FilePath import :: FilePath
from iTasks.Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld

:: StoreNamespace	:== String
:: StoreName		:== String
:: StorePrefix		:== String

// Predefined namespaces
NS_TASK_INSTANCES		:== "task-instances"
NS_DOCUMENT_CONTENT		:== "document-data"
NS_APPLICATION_SHARES	:== "application-data"
NS_JAVASCRIPT_CACHE     :== "js-cache"



/**
* Create a shared data source for a piece of data in the store
*
* @param The namespace in the store
* @param The key of the value in the  store
* @param Optionally a default value to be used on first read. If nothing is given an error will occur when reading before writing.
*
* @return The shared data source
*/
storeAccess :: !StoreNamespace !StoreName !(Maybe a) -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

/**
* Determine the location of the store from data directory and build
*/
storePath :: !FilePath !String -> FilePath

/**
* Store a value in the default format
*/
storeValue				:: !StoreNamespace !StoreName !a				!*IWorld -> *IWorld							| JSONEncode{|*|}, TC a

/**
* Load a value from the store
*/
loadValue				:: !StoreNamespace !StoreName			!*IWorld -> (!Maybe a,!*IWorld)				| JSONDecode{|*|}, TC a

/**
* Deletes the value with given key from the store
*/
deleteValue				:: !StoreNamespace !StoreName				!*IWorld -> *IWorld

/**
* Deletes all values that start with the prefix from the store
*/
deleteValues			:: !StoreNamespace !StorePrefix				!*IWorld -> *IWorld

/**
* Store a binary blob
*/
storeBlob				:: !StoreNamespace !StoreName !{#Char}		!*IWorld -> *IWorld

/**
* Load a binary blob
*/
loadBlob				:: !StoreNamespace !StoreName 				!*IWorld -> (!Maybe {#Char}, !*IWorld)

/**
* List the keys for a given namespace
*/
listStores              :: !StoreNamespace                          !*IWorld -> (![StoreName], !*IWorld)

