implementation module Shared

import StdTuple, StdFunc, Void, Maybe, Time, Error, GenUpdate, Util, Functor, StdList
from Types import :: IWorld

readShared :: !(Shared r w) !*IWorld -> (!MaybeErrorString r,!*IWorld)
readShared (Shared read _ _) iworld = read iworld

writeShared :: !(Shared r w) !w !*IWorld -> (!MaybeErrorString Void,!*IWorld)
writeShared (Shared _ write _) val iworld = write val iworld

getSharedTimestamp :: !(Shared r w) !*IWorld -> (!MaybeErrorString Timestamp,!*IWorld)
getSharedTimestamp (Shared _ _ getTimestamp) iworld = getTimestamp iworld

mapSharedRead :: !(r -> r`) !(Shared r w) -> (Shared r` w)
mapSharedRead f (Shared read write getTimestamp) = Shared (appFst (fmap f) o read) write getTimestamp

mapSharedWrite :: !(w` r -> w) !(Shared r w) -> (Shared r w`)
mapSharedWrite f (Shared read write getTimestamp) = Shared read newWrite getTimestamp
where
	newWrite v iworld
		# (m,iworld) = read iworld
		| isError m = (liftError m,iworld)
		= write (f v (fromOk m)) iworld
		
mapShared :: !(r -> r`,w` r -> w) !(Shared r w) -> Shared r` w`
mapShared (readMap,writeMap) shared = mapSharedRead readMap (mapSharedWrite writeMap shared)

isSharedChanged :: !(Shared r w) !Timestamp !*IWorld -> (!MaybeErrorString Bool,!*IWorld)
isSharedChanged (Shared _ _ getTimestamp) t0 iworld
	# (t1,iworld) = getTimestamp iworld
	| isError t1 = (liftError t1,iworld)
	= (Ok (t0 < fromOk t1),iworld)

toReadOnlyShared :: !(Shared r w) -> ReadOnlyShared r
toReadOnlyShared (Shared read write getTimestamp) = Shared read (\_ iworld -> (Ok Void,iworld)) getTimestamp

(>+<) infixl 6 :: !(Shared r0 w0) !(Shared r1 w1) -> Shared (r0,r1) (w0,w1)
(>+<) (Shared read0 write0 getTimestamp0) (Shared read1 write1 getTimestamp1)
	= Shared (composeReads read0 read1) (composeWrites write0 write1) (composeGetTimestamps getTimestamp0 getTimestamp1)
	
(>+|) infixl 6 :: !(Shared r0 w0) !(Shared r1 w1) -> Shared (r0,r1) w0
(>+|) (Shared read0 write0 getTimestamp0) (Shared read1 _ getTimestamp1)
	= Shared (composeReads read0 read1) write0 (composeGetTimestamps getTimestamp0 getTimestamp1)

(|+<) infixl 6 :: !(Shared r0 w0) !(Shared r1 w1) -> Shared (r0,r1) w1
(|+<) (Shared read0 _ getTimestamp0) (Shared read1 write1 getTimestamp1)
	= Shared (composeReads read0 read1) write1 (composeGetTimestamps getTimestamp0 getTimestamp1)
	
(|+|) infixl 6 :: !(Shared r0 w0) !(Shared r1 w1) -> ReadOnlyShared (r0,r1)
(|+|) (Shared read0 write0 getTimestamp0) (Shared read1 write1 getTimestamp1)
	= Shared (composeReads read0 read1) (\_ iworld -> (Ok Void,iworld)) (composeGetTimestamps getTimestamp0 getTimestamp1)
	
(>&<) infixl 6 :: !(SymmetricShared a) !(SymmetricShared b) -> (SymmetricShared (a,b)) | gEq{|*|} a & gEq{|*|} b
(>&<) (Shared read0 write0 getTimestamp0) (Shared read1 write1 getTimestamp1)
	= Shared (composeReads read0 read1) writeC (composeGetTimestamps getTimestamp0 getTimestamp1)
where
	writeC (na,nb) iworld
		# (oa,iworld)	= read0 iworld
		| isError oa = (liftError oa,iworld)
		# (ob,iworld)	= read1 iworld
		| isError ob = (liftError ob,iworld)
		# (r,iworld)	= if (fromOk oa =!= na) (write0 na iworld) (Ok Void,iworld)
		| isError r = (liftError r,iworld)
		= if (fromOk ob =!= nb) (write1 nb iworld) (Ok Void,iworld)

composeReads						= compose (\a b -> (a,b))
composeGetTimestamps				= compose max
composeWrites write0 write1 (v0,v1)	= compose const (write0 v0) (write1 v1)

compose compF f0 f1 iworld
	# (res0,iworld)	= f0 iworld
	| isError res0	= (liftError res0,iworld)
	# (res1,iworld)	= f1 iworld
	| isError res1	= (liftError res1,iworld)
	= (Ok (compF (fromOk res0) (fromOk res1)),iworld)
	
symmetricLens :: !(a b -> b) !(b a -> a) !(SymmetricShared a) !(SymmetricShared b) -> (!SymmetricShared a,!SymmetricShared b)
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB 
	newSharedA = mapShared (fst,\a (_,b) -> (a,putr a b)) sharedAll
	newSharedB = mapShared (snd,\b (a,_) -> (putl b a,b)) sharedAll

makeReadOnlyShared :: !(*IWorld -> *(!a,!*IWorld)) -> ReadOnlyShared a
makeReadOnlyShared valueF = Shared (appFst Ok o valueF) roWrite roGetTimestamp

makeReadOnlySharedError	:: !(*IWorld -> *(!MaybeErrorString a,!*IWorld))	-> ReadOnlyShared a
makeReadOnlySharedError valueF = Shared valueF roWrite roGetTimestamp

roWrite _ iworld = (Ok Void,iworld)
roGetTimestamp iworld=:{IWorld|timestamp} = (Ok timestamp,iworld)
