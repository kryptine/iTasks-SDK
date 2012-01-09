implementation module Shared

import StdTuple, StdFunc, Void, Maybe, Time, Error, GenUpdate, Util, Functor, StdList, Tuple
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

maybeUpdateShared :: !(ReadWriteShared r w) !(r -> Maybe w) !*IWorld -> (!MaybeErrorString (Maybe w),!*IWorld)
maybeUpdateShared shared=:(ReadWriteShared _ read _ _) updF iworld
	# (val,iworld)	= read iworld
	| isError val	= (liftError val,iworld)
	# mbwval		= updF (fromOk val)
	= case mbwval of
		Nothing	= (Ok Nothing,iworld)
		Just wval
			# (wres,iworld)	= writeShared shared wval iworld
			| isError wres	= (liftError wres,iworld)
			= (Ok (Just wval),iworld)

getSharedVersion :: !(ReadWriteShared r w) !*IWorld -> (!MaybeErrorString Int,!*IWorld)
getSharedVersion (ReadWriteShared _ _ _ getVersion) iworld = getVersion iworld

isSharedChanged :: !(ReadWriteShared r w) !Int !*IWorld -> (!MaybeErrorString Bool,!*IWorld)
isSharedChanged (ReadWriteShared _ _ _ getVersion) v0 iworld
	# (v1,iworld) = getVersion iworld
	| isError v1 = (liftError v1,iworld)
	= (Ok (v0 < fromOk v1),iworld)

makeReadOnlyShared :: !SharedId !(*IWorld -> *(!a,!*IWorld)) !(*IWorld -> *(!Int,!*IWorld)) -> ReadOnlyShared a
makeReadOnlyShared id valueF tsF = ReadWriteShared [id] (appFst Ok o valueF) roWrite (appFst Ok o tsF)

makeReadOnlySharedError	:: !SharedId !(*IWorld -> *(!MaybeErrorString a,!*IWorld)) !(*IWorld -> *(!MaybeErrorString Int,!*IWorld)) -> ReadOnlyShared a
makeReadOnlySharedError id valueF tsF = ReadWriteShared [id] valueF roWrite tsF

roWrite _ iworld = (Ok Void,iworld)
