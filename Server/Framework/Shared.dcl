definition module Shared
/**
* This module provides references to shared data with separate read and write type.
* Each shared reference provides a timestamp of its last change.
* Functional mappings can be used to change the types of references.
* Additonally references can be composed.
*/

import Void, Maybe, Error, GenEq
from StdFunc	import id, const
from Types		import :: IWorld
from Time		import :: Timestamp
from GenUpdate	import :: USt, :: UpdateMode, generic gUpdate

// shared reference, data of type r can be read, data of type w can be written
// each shared has at least one id, composed shares have the ids of all their components
:: ReadWriteShared r w = ReadWriteShared ![SharedId] !(SharedRead r) !(SharedWrite w) !SharedGetTimestamp
// special cases for symmetric shareds (same read/write type) & read only shareds
:: Shared			a :== ReadWriteShared a a
:: ReadOnlyShared	a :== ReadWriteShared a Void
:: SharedId :== String

// functions used for defining the basic operations provided by shared references
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
readShared :: !(ReadWriteShared r w) !*IWorld -> (!MaybeErrorString r,!*IWorld)

/**
* Writes data to the shared location.
*
* @param A reference to shared data
* @param The value to write
* @param iworld
* @return Possibly a string error
*/
writeShared :: !(ReadWriteShared r w) !w !*IWorld -> (!MaybeErrorString Void,!*IWorld)

/**
* Atomically updates shared data.
*
* @param A reference to shared data
* @param The update function
* @param iworld
* @return The value written to the shared
*/
updateShared :: !(ReadWriteShared r w) !(r -> w) !*IWorld -> (!MaybeErrorString w,!*IWorld)

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapSharedRead	:: !(r -> r`)				!(ReadWriteShared r w) -> ReadWriteShared r` w
mapSharedWrite	:: !(w` r -> w)				!(ReadWriteShared r w) -> ReadWriteShared r w`
mapShared		:: !(!r -> r`,!w` r -> w)	!(ReadWriteShared r w) -> ReadWriteShared r` w`

/**
* Gets the timestamp of the last change.
*
* @param A reference to shared data
* @param iworld
* @return timestamp of last change (or a string error)
*/
getSharedTimestamp :: !(ReadWriteShared r w) !*IWorld -> (!MaybeErrorString Timestamp,!*IWorld)

/**
* Checks if the store's value has been changed since given timestamp.
*
* @param A reference to shared data
* @param A timestamp
* @param iworld
* @return A flag indicating if the data has been changed (or a string error)
*/
isSharedChanged :: !(ReadWriteShared r w) !Timestamp !*IWorld -> (!MaybeErrorString Bool,!*IWorld)

/**
* Converts a shared reference to a read only shared.
* Writing to a read only shared has no effect.
*
* @param A reference to shared data
* @return A read-only reference to shared data providing the same data, but with write type Void
*/
toReadOnlyShared :: !(ReadWriteShared r w) -> ReadOnlyShared r

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>+<) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadWriteShared (r0,r1) (w0,w1)
(>+|) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadWriteShared (r0,r1) w0
(|+<) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadWriteShared (r0,r1) w1
(|+|) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadOnlyShared (r0,r1)

// Compose symmetric shared references and only write to one of the shares if the value changed (read shared <> value to write).
(>&<) infixl 6 :: !(Shared a) !(Shared b) -> (Shared (a,b)) | gEq{|*|} a & gEq{|*|} b

/**
* Puts a symmetric lens between two symmetric shared data sources.
* Changes of one also affects the other one.
*
* @param putr: used to map changes of shared a to shared b
* @param putl: used to map changes of shared b to shared a
* @param SymmetricShared a
* @param SymmetricShared b
* @param ReadWriteShared references of the same type with symmetric lens between them
*/
symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a) !(Shared b) -> (!Shared a,!Shared b)

/**
* Creates a read-only shared which's value & timestamp is computed by a function on IWorld (which can optionally give an error).
*/
makeReadOnlyShared		:: !SharedId  !(*IWorld -> *(!a,!*IWorld))						!(*IWorld -> *(!Timestamp,!*IWorld))					-> ReadOnlyShared a
makeReadOnlySharedError	:: !SharedId  !(*IWorld -> *(!MaybeErrorString a,!*IWorld))	!(*IWorld -> *(!MaybeErrorString Timestamp,!*IWorld))	-> ReadOnlyShared a