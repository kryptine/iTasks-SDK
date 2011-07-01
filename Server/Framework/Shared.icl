implementation module Shared

import StdTuple, StdFunc, Void, Maybe, Time, Error, GenUpdate, Util, Functor, StdList
from IWorld import :: IWorld(..), :: Control, :: ProcessId
from SharedCombinators import :: ReadOnlyShared

readShared :: !(ReadWriteShared r w) !*IWorld -> (!MaybeErrorString r,!*IWorld)
readShared (ReadWriteShared myIds read _ _) iworld=:{readShares}
	# readShares = case readShares of
		Nothing		= Nothing
		Just ids	= Just (removeDup (ids ++ myIds))
	= read {iworld & readShares = readShares}

writeShared :: !(ReadWriteShared r w) !w !*IWorld -> (!MaybeErrorString Void,!*IWorld)
writeShared (ReadWriteShared myIds _ write _) val iworld=:{readShares}
	# readShares = case readShares of
		Nothing		= Nothing
		Just ids
			| isAnyMember myIds ids	= Nothing
			| otherwise				= readShares
	= write val {iworld & readShares = readShares}

updateShared :: !(ReadWriteShared r w) !(r -> w) !*IWorld -> (!MaybeErrorString w,!*IWorld)
updateShared shared=:(ReadWriteShared _ read _ _) updF iworld
	# (val,iworld)	= read iworld
	| isError val	= (liftError val,iworld)
	# wval			= updF (fromOk val)
	# (wres,iworld)	= writeShared shared wval iworld
	| isError wres	= (liftError wres,iworld)
	= (Ok wval,iworld)

getSharedTimestamp :: !(ReadWriteShared r w) !*IWorld -> (!MaybeErrorString Timestamp,!*IWorld)
getSharedTimestamp (ReadWriteShared _ _ _ getTimestamp) iworld = getTimestamp iworld

isSharedChanged :: !(ReadWriteShared r w) !Timestamp !*IWorld -> (!MaybeErrorString Bool,!*IWorld)
isSharedChanged (ReadWriteShared _ _ _ getTimestamp) t0 iworld
	# (t1,iworld) = getTimestamp iworld
	| isError t1 = (liftError t1,iworld)
	= (Ok (t0 < fromOk t1),iworld)

makeReadOnlyShared :: !SharedId !(*IWorld -> *(!a,!*IWorld)) !(*IWorld -> *(!Timestamp,!*IWorld)) -> ReadOnlyShared a
makeReadOnlyShared id valueF tsF = ReadWriteShared [id] (appFst Ok o valueF) roWrite (appFst Ok o tsF)

makeReadOnlySharedError	:: !SharedId !(*IWorld -> *(!MaybeErrorString a,!*IWorld)) !(*IWorld -> *(!MaybeErrorString Timestamp,!*IWorld)) -> ReadOnlyShared a
makeReadOnlySharedError id valueF tsF = ReadWriteShared [id] valueF roWrite tsF

roWrite _ iworld = (Ok Void,iworld)