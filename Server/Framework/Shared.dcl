definition module Shared
/**
* This module provides references to shared data with separate read and write type.
* Each shared reference provides a timestamp of its last change.
* Functional mappings can be used to change the types of references.
* Additonally references can be composed.
*/

import Void, Maybe, Error, GenEq
from StdFunc	import id, const
from IWorld		import :: IWorld
from Time		import :: Timestamp
from GenUpdate	import :: USt, :: UpdateMode, generic gUpdate
from SharedCombinators import :: ReadOnlyShared

// shared reference, data of type r can be read, data of type w can be written
// each shared has at least one id, composed shares have the ids of all their components
:: ReadWriteShared r w = ReadWriteShared ![SharedId] !(SharedRead r) !(SharedWrite w) !SharedGetTimestamp

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
* Atomically updates shared data only if an appropriate value is available
* @param A reference to shared data
* @param The update function
* @param iworld
* @return The value written to the shared or nothing when there was no write
*/
maybeUpdateShared :: !(ReadWriteShared r w) !(r -> Maybe w) !*IWorld -> (!MaybeErrorString (Maybe w),!*IWorld)

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
* Creates a read-only shared which's value & timestamp is computed by a function on IWorld (which can optionally give an error).
*/
makeReadOnlyShared		:: !SharedId  !(*IWorld -> *(!a,!*IWorld))						!(*IWorld -> *(!Timestamp,!*IWorld))					-> ReadOnlyShared a
makeReadOnlySharedError	:: !SharedId  !(*IWorld -> *(!MaybeErrorString a,!*IWorld))	!(*IWorld -> *(!MaybeErrorString Timestamp,!*IWorld))	-> ReadOnlyShared a