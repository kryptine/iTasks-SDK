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
import GenPrint
import GenParse

// Abstract store
:: *Store

// Storage format
:: StoreFormat = SFPlain | SFDynamic

/**
* Create a store
*/
createStore		:: !String -> *Store

/**
* Store a value in the default format
* Values are first stored in a cache, so no world is needed yet
*/
storeValue		:: !String !a !*Store	-> *Store | gPrint{|*|}, TC a

/**
* Store a value in a specific format
*/
storeValueAs	:: 	!StoreFormat !String !a !*Store	-> *Store	| gPrint{|*|}, TC a

/**
* Load a value from the store
*/
loadValue		:: !String !*Store !*World -> (!Maybe a, !*Store, !*World) | gParse{|*|}, TC a

/**
* Loads a dynamic from the store without unwrapping it
*/
loadDynamicValue :: !String !*Store !*World -> (!Maybe Dynamic, !*Store, !*World)

/**
* Deletes all values that start with the prefix from the store
*/
deleteValues	:: !String !*Store !*World -> (!*Store, !*World)

/**
* Copies all values in the store that start with the first prefix to
* a new name where the first prefix is replaced by the second.
*/
copyValues		:: !String !String !*Store !*World -> (!*Store, !*World)

/**
* Writes all values stored in the cache to disk
*/
flushCache		:: !*Store !*World -> (!*Store,!*World)