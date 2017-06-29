definition module iTasks._Framework.Store
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
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.Maybe import :: Maybe
from Data.Error import :: MaybeError, :: MaybeErrorString
from System.Time import :: Timestamp
from System.FilePath import :: FilePath
from iTasks._Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks._Framework.IWorld import :: IWorld
from iTasks._Framework.Generic				import class iTask
from iTasks.UI.Editor import :: Editor, :: EditMask, :: Masked, :: VSt
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks._Framework.Generic.Visualization	import generic gText, :: TextFormat(..), toMultiLineText
from iTasks._Framework.Generic.Defaults		import generic gDefault
from GenEq import generic gEq

:: StoreNamespace	:== String
:: StoreName		:== String
:: StorePrefix		:== String
:: BuildID          :== String

// Predefined namespaces
NS_TASK_INSTANCES		:== "task-instances"
NS_DOCUMENT_CONTENT		:== "document-data"
NS_APPLICATION_SHARES	:== "application-data"
NS_JAVASCRIPT_CACHE     :== "js-cache"

:: StoreReadError
    = StoreReadMissingError !StoreName      //When there is no file on disk for this
    | StoreReadDataError !StoreName         //When there is a problem reading data from disk
    | StoreReadTypeError !StoreName         //When the data cannot be decoded based on the type
    | StoreReadBuildVersionError !StoreName //When there is a stored value but it has the wrong build version

instance toString StoreReadError
derive class iTask StoreReadError

/**
* Creates a store in memory. Values in this store are lost when the server shuts down.
*
* @param The namespace in the store
* @param Optionally a default content to be used on first read. If nothing is given an error will occur when reading before writing.
*/
memoryStore   :: !StoreNamespace !(Maybe a) -> RWShared StoreName a a | TC a

/**
* Creates a 'raw' store which keeps values in multiple files indexed by a store name
* The application's build ID is automatically stored with the content, and returned when reading
*
* @param The namespace in the store
* @param Automatically reset the the store if an error occurs
* @param Optionally a default content to be used on first read. If nothing is given an error will occur when reading before writing.
*/
fullFileStore :: !StoreNamespace !Bool !(Maybe {#Char}) -> RWShared StoreName (!BuildID,!{#Char}) {#Char}

/**
* Extends a fullFileStore with JSON encoding/decoding such that arbitrary values can be stored.
* It also adds optional buildID checking to make sure that JSONEncoded functions and dynamics are
* not decoded if the versions don't match.
*
* @param The namespace in the store
* @param Check the build versions to protect against deserializing outdated functions stored by older versions
* @param Automatically reset the the store if an error occurs
* @param Optionally a default content to be used on first read. If nothing is given an error will occur when reading before writing.
*/
jsonFileStore :: !StoreNamespace !Bool !Bool !(Maybe a) -> RWShared StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

/**
* Optimized caching version of the jsonFileStore.
* During the evaluation of a task instance, the shares that are read from disk are kept in memory,
* writes are applied to the in-memory version, and json encoding and writing to disk is deferred.
*
* @param The namespace in the store
* @param Check the build versions to protect against deserializing outdated functions stored by older versions
* @param Automatically reset the the store if an error occurs
* @param Keep the value in the cache between evaluations
* @param Optionally a default content to be used on first read. If nothing is given an error will occur when reading before writing.
*/
cachedJSONFileStore :: !StoreNamespace !Bool !Bool !Bool !(Maybe a) -> RWShared StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

/**
* Extends a fullFileStore with dynamic string encoding such that arbitrary values can be stored.
* This encoding can be significantly more efficient for storing large functions.
* Additionally, caching is applied as for cachedJSONFileStore.
* During the evaluation of a task instance, the shares that are read from disk are kept in memory,
* writes are applied to the in-memory version, and encoding and writing to disk is deferred.
*
* @param The namespace in the store
* @param Check the build versions to protect against deserializing outdated functions stored by older versions
* @param Automatically reset the the store if an error occurs
* @param Keep the value in the cache between evaluations
* @param Optionally a default content to be used on first read. If nothing is given an error will occur when reading before writing.
*/
cachedDynamicStringFileStore :: !StoreNamespace !Bool !Bool !Bool !(Maybe a) -> RWShared StoreName a a | TC a

/**
* This function is called at the very end of the evaluation of a task instance.
* It writes all pending writes to disk and clears values that are no longer needed from memory.
*/
flushShareCache :: *IWorld -> *IWorld

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
deleteValue             :: !StoreNamespace !StoreName !*IWorld -> *(MaybeErrorString (),*IWorld)

/**
* Deletes all values that start with the prefix from the store
*/
deleteValues 			:: !StoreNamespace !StorePrefix !*IWorld -> *(MaybeErrorString (),*IWorld)

/**
* List the namespaces in the store
*/
listStoreNamespaces     ::                                          !*IWorld -> (![StoreNamespace], !*IWorld)
/**
* List the keys for a given namespace
*/
listStoreNames          :: !StoreNamespace                          !*IWorld -> (!MaybeErrorString [StoreName], !*IWorld)

/**
* Delete all values in the store
*/
emptyStore :: !*IWorld -> *IWorld

//writeToDisk :: !StoreNamespace !StoreName !String !*IWorld -> *IWorld
writeToDisk :: !StoreNamespace !StoreName !String !*IWorld -> (MaybeErrorString (), *IWorld)

readFromDisk :: !StoreNamespace !StoreName !*IWorld -> (MaybeError StoreReadError (!BuildID, !String), !*IWorld)
