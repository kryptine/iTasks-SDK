implementation module Shared

import IWorld, Void, Error, StdFunc, StdMisc, Tuple, Maybe
from SharedDataSource import :: RWShared, :: Version, createBasicDataSource, :: BasicSourceOps{..}, :: OBSERVER

makeUnsafeShare ::
	!String
	!String
	!(*IWorld      -> *(!MaybeErrorString a,!*IWorld))
	!(a *IWorld    -> *(!MaybeErrorString Void,!*IWorld))
	!(*IWorld      -> *(!MaybeErrorString Version,!*IWorld))
	->
	Shared a
makeUnsafeShare type ident read write getVersion
	= createBasicDataSource type ident (\iworld -> (ops, iworld)) id const
where
	ops =	{ read			= combineErrorsSt read getVersion (\r ver -> Ok (r,ver))
			, write			= write
			, getVersion	= getVersion
			, lock			= id
			, lockExcl		= id
			, unlock		= id
			, close			= id
			, addObserver	= \_ _ -> abort "unsafe share: not implemented"
			}
		
makeReadOnlyShared ::
	!String
	!String
	!(*IWorld      -> *(!a,!*IWorld))
	!(*IWorld      -> *(!Version,!*IWorld))
	->
	ReadOnlyShared a
makeReadOnlyShared type ident read getVersion
	= makeReadOnlySharedError type ident (appFst Ok o read) (appFst Ok o getVersion)
	
makeReadOnlySharedError ::
	!String
	!String
	!(*IWorld      -> *(!MaybeErrorString a,!*IWorld))
	!(*IWorld      -> *(!MaybeErrorString Version,!*IWorld))
	->
	ReadOnlyShared a
makeReadOnlySharedError type ident read getVersion
	= createBasicDataSource type ident (\iworld -> (ops, iworld)) id (\_ b -> b)
where
	ops =	{ read			= combineErrorsSt read getVersion (\r ver -> Ok (r,ver))
			, write			= \_ env -> (Ok Void, env)
			, getVersion	= getVersion
			, lock			= id
			, lockExcl		= id
			, unlock		= id
			, close			= id
			, addObserver	= \_ _ -> abort "unsafe share: not implemented"
			}