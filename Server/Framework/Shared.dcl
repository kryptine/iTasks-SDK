definition module Shared
/**
* This module provides references to shared data with separate read and write type.
* Each shared reference provides a timestamp of its last change.
* Functional mappings can be used to change the types of references.
* Additonally references can be composed.
*/

import Void, Maybe, Error
from StdFunc	import id, const
from Types		import :: IWorld
from Time		import :: Timestamp
from GenUpdate	import generic gUpdate

// shared reference, data of type r can be read, data of type w can be written
:: Shared r w = Shared !(SharedRead r) !(SharedWrite w) !SharedGetTimestamp
// special cases for symmetric shareds (same read/write type) & read only shareds
:: SymmetricShared	a :== Shared a a
:: ReadOnlyShared	a :== Shared a Void

// functions used for the defining the basic operations provided by shared references
:: SharedRead			a :== (*IWorld		-> *(!MaybeErrorString a,!*IWorld))
:: SharedWrite			a :== (a *IWorld	-> *(!MaybeErrorString Void,!*IWorld))
:: SharedGetTimestamp	  :== (*IWorld		-> *(!MaybeErrorString Timestamp,!*IWorld))

/**
* Reads shared data.
*
* @param A reference to shared data
* @param iworld
* @return The stored value (or a string error)
*/
readShared :: !(Shared r w) !*IWorld -> (!MaybeErrorString r,!*IWorld)

/**
* Writes data to the shared location.
*
* @param A reference to shared data
* @param The value to write
* @param iworld
* @return Possibly a string error
*/
writeShared :: !(Shared r w) !w !*IWorld -> (!MaybeErrorString Void,!*IWorld)

/**
* Maps the read type, the write type or both of a shared reference to another one using a funcitonal mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapSharedRead	:: !(r -> r`)			!(Shared r w) -> Shared r` w
mapSharedWrite	:: !(w` r -> w)			!(Shared r w) -> Shared r w`
mapShared		:: !(r -> r`,w` r -> w)	!(Shared r w) -> Shared r` w`

/**
* Gets the timestamp of the last change.
*
* @param A reference to shared data
* @param iworld
* @return timestamp of last change (or a string error)
*/
getSharedTimestamp :: !(Shared r w) !*IWorld -> (!MaybeErrorString Timestamp,!*IWorld)

/**
* Checks if the store's value has been changed since given timestamp.
*
* @param A reference to shared data
* @param A timestamp
* @param iworld
* @return A flag indicating if the data has been changed (or a string error)
*/
isSharedChanged :: !(Shared r w) !Timestamp !*IWorld -> (!MaybeErrorString Bool,!*IWorld)

/**
* Converts a shared reference to a read only shared.
* Writing to a read only shared has no effect.
*
* @param A reference to shared data
* @return A read-only reference to shared data providing the same data, but with write type Void
*/
toReadOnlyShared :: !(Shared r w) -> ReadOnlyShared r

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types or only one of them.
(>+<) infixl 6 :: !(Shared r0 w0) !(Shared r1 w1) -> Shared (r0,r1) (w0,w1)
(>+|) infixl 6 :: !(Shared r0 w0) !(Shared r1 w1) -> Shared (r0,r1) w0
(|+<) infixl 6 :: !(Shared r0 w0) !(Shared r1 w1) -> Shared (r0,r1) w1

/**
* Creates a simple read-only shared which's value is computed by a function on IWorld (which can optionally give an error).
* The timestamp of the shared is the current one.
*/
makeReadOnlyShared		:: !(*IWorld -> *(!a,!*IWorld))						-> ReadOnlyShared a
makeReadOnlySharedError	:: !(*IWorld -> *(!MaybeErrorString a,!*IWorld))	-> ReadOnlyShared a
