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
:: ReadWriteShared r w = ReadWriteShared ![SharedId] !(SharedRead r) !(SharedWrite w) !SharedGetVersion

:: SharedId :== String

// functions used for defining the basic operations provided by shared references
:: SharedRead			a :== (*IWorld		-> *(!MaybeErrorString a,!*IWorld))
:: SharedWrite			a :== (a *IWorld	-> *(!MaybeErrorString Void,!*IWorld))
:: SharedGetVersion       :== (*IWorld		-> *(!MaybeErrorString Int,!*IWorld))

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
* Gets the curret version of the shared data.
*
* @param A reference to shared data
* @param iworld
* @return version of last change (or a string error)
*/
getSharedVersion :: !(ReadWriteShared r w) !*IWorld -> (!MaybeErrorString Int,!*IWorld)

/**
* Checks if the store's value has been changed since given version.
*
* @param A reference to shared data
* @param A version
* @param iworld
* @return A flag indicating if the data has been changed (or a string error)
*/
isSharedChanged :: !(ReadWriteShared r w) !Int !*IWorld -> (!MaybeErrorString Bool,!*IWorld)

/**
* Creates a read-only shared which's value & timestamp is computed by a function on IWorld (which can optionally give an error).
*/
makeReadOnlyShared		:: !SharedId  !(*IWorld -> *(!a,!*IWorld))						!(*IWorld -> *(!Int,!*IWorld))					-> ReadOnlyShared a
makeReadOnlySharedError	:: !SharedId  !(*IWorld -> *(!MaybeErrorString a,!*IWorld))	!(*IWorld -> *(!MaybeErrorString Int,!*IWorld))	-> ReadOnlyShared a
