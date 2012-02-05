definition module Shared

import IWorld, Void, Error
from SharedDataSource import :: RWShared, :: Version, null, ::ROShared, :: WOShared, mapRead, mapWrite, mapReadWrite, mapReadError, mapWriteError, mapReadWriteError, toReadOnly, >+<, >+|, |+<, |+|

:: ReadWriteShared r w	:== RWShared r w IWorld
:: Shared a				:== ReadWriteShared a a
:: ReadOnlyShared a		:== ReadWriteShared a Void
:: WriteOnlyShared a	:== ReadWriteShared Void a

makeUnsafeShare ::
	!String
	!String
	!(*IWorld      -> *(!MaybeErrorString a,!*IWorld))
	!(a *IWorld    -> *(!MaybeErrorString Void,!*IWorld))
	!(*IWorld      -> *(!MaybeErrorString Version,!*IWorld))
	->
	Shared a
	
makeReadOnlyShared ::
	!String
	!String
	!(*IWorld      -> *(!a,!*IWorld))
	!(*IWorld      -> *(!Version,!*IWorld))
	->
	ReadOnlyShared a
	
makeReadOnlySharedError ::
	!String
	!String
	!(*IWorld      -> *(!MaybeErrorString a,!*IWorld))
	!(*IWorld      -> *(!MaybeErrorString Version,!*IWorld))
	->
	ReadOnlyShared a