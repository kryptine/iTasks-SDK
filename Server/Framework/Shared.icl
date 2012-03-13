implementation module Shared

import IWorld, Void, Error, StdFunc, StdMisc, Tuple, Maybe, StdString
from SharedDataSource import :: RWShared, :: Hash, createBasicDataSource, :: BasicSourceOps{..}, :: OBSERVER, read, createProxyDataSource

makeUnsafeShare ::
	!String
	!String
	!(*IWorld      -> *(!MaybeErrorString a,!*IWorld))
	!(a *IWorld    -> *(!MaybeErrorString Void,!*IWorld))
	->
	Shared a
makeUnsafeShare type ident read write
	= createBasicDataSource type ident (\iworld -> (ops, iworld)) id const
where
	ops =	{ read			= read
			, write			= write
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
	->
	ReadOnlyShared a
makeReadOnlyShared type ident read
	= makeReadOnlySharedError type ident (appFst Ok o read)
	
makeReadOnlySharedError ::
	!String
	!String
	!(*IWorld      -> *(!MaybeErrorString a,!*IWorld))
	->
	ReadOnlyShared a
makeReadOnlySharedError type ident read
	= createBasicDataSource type ident (\iworld -> (ops, iworld)) id (\_ b -> b)
where
	ops =	{ read			= read
			, write			= \_ env -> (Ok Void, env)
			, lock			= id
			, lockExcl		= id
			, unlock		= id
			, close			= id
			, addObserver	= \_ _ -> abort "unsafe share: not implemented"
			}
			
(>+>) infixl 6 :: !(ReadWriteShared r0 w0) !(r0 -> (ReadWriteShared r1 w1)) -> ReadWriteShared r1 w1
(>+>) share shareGenF = createProxyDataSource genShare id const
where
	genShare env
		# (mbeRes, env) = read share env
		= case mbeRes of
			Error e		= abort ("proxy data source: " +++ e)
			Ok (r,_)	= (shareGenF r, env)
	