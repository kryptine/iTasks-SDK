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
from Data.Error import :: MaybeError, :: MaybeErrorString
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

:: StoreReadError
    = StoreReadMissingError         //When there is no file on disk for this
    | StoreReadDataError            //When there is a problem reading data from disk
    | StoreReadTypeError            //When the data cannot be decoded based on the type
    | StoreReadBuildVersionError    //When there is a stored value but it has the wrong build version

instance toString StoreReadError

/**
* Create a shared data source for a piece of data in the store
*
* @param The namespace in the store
* @param The key of the value in the  store
* @param Check the build versions to protect against deserializing outdated functions stored by older versions
*        of the executable (if in doubt, use True)
* @param Automatically reset the the store if an error occurs
* @param Optionally a default value to be used on first read. If nothing is given an error will occur when reading before writing.
*
* @return The shared data source
*/
singleValueStoreSDS :: !StoreNamespace !StoreName !Bool !Bool !(Maybe a) -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

/**
* Load a value from the store
* @param The namespace
* @param The name of the store
* @param Check the build versions to protect against deserializing outdated functions stored by older versions
*        of the executable (if in doubt, use True)
* @param The value to write
*/
singleValueStoreRead :: !StoreNamespace !StoreName !Bool		!*IWorld -> (!MaybeError StoreReadError a,!*IWorld)				| JSONDecode{|*|}, TC a
/**
* Write a value to a store
* @param The namespace
* @param The name of the store
* @param Check the build versions to protect against deserializing outdated functions stored by older versions
*        of the executable (if in doubt, use True)
* @param The value to write
* @param Write build version to enable version check
*/
singleValueStoreWrite :: !StoreNamespace !StoreName !Bool !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a

/**
* Store a binary blob
*/
blobStoreWrite	:: !StoreNamespace !StoreName !{#Char}		!*IWorld -> *IWorld

/**
* Load a binary blob
*/
blobStoreRead	:: !StoreNamespace !StoreName 				!*IWorld -> (!MaybeError StoreReadError {#Char}, !*IWorld)


/**
* Deletes the value with given key from the store
*/
deleteValue				:: !StoreNamespace !StoreName				!*IWorld -> *IWorld

/**
* Deletes all values that start with the prefix from the store
*/
deleteValues			:: !StoreNamespace !StorePrefix				!*IWorld -> *IWorld

/**
* List the namespaces in the store
*/
listStoreNamespaces     ::                                          !*IWorld -> (![StoreNamespace], !*IWorld)
/**
* List the keys for a given namespace
*/
listStoreNames          :: !StoreNamespace                          !*IWorld -> (!MaybeErrorString [StoreName], !*IWorld)

