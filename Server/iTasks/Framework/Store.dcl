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
from System.Time import :: Timestamp
from System.FilePath import :: FilePath
from iTasks.Framework.SDS import :: RWShared
from iTasks.Framework.IWorld import :: IWorld

// Storage formats
:: StoreFormat = SFPlain | SFDynamic

:: StoreKey			:== String
:: StoreNamespace	:== String
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
storeAccess :: !StoreNamespace !StoreKey !(Maybe a) -> RWShared a a IWorld | JSONEncode{|*|}, JSONDecode{|*|}, TC a

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
* Deletes all values that start with the prefix from the store
*/
deleteValues			:: !StoreNamespace !StorePrefix				!*IWorld -> *IWorld

/**
* Store a binary blob
*/
storeBlob				:: !StoreNamespace !StoreKey !{#Char}		!*IWorld -> *IWorld

/**
* Load a binary blob
*/
loadBlob				:: !StoreNamespace !StoreKey 				!*IWorld -> (!Maybe {#Char}, !*IWorld)

/**
* List the keys for a given namespace
*/
listKeys                :: !StoreNamespace                          !*IWorld -> (![StoreKey], !*IWorld)

