implementation module SharedCombinators

import StdTuple, StdList, StdFunc, Util, Shared, Time, Tuple, Error

mapSharedRead :: !(r -> r`) !(ReadWriteShared r w) -> (ReadWriteShared r` w)
mapSharedRead f (ReadWriteShared id read write getTimestamp) = ReadWriteShared id (appFst (fmap f) o read) write getTimestamp

mapSharedWrite :: !(w` r -> w) !(ReadWriteShared r w) -> (ReadWriteShared r w`)
mapSharedWrite f (ReadWriteShared id read write getTimestamp) = ReadWriteShared id read newWrite getTimestamp
where
	newWrite v iworld
		# (m,iworld) = read iworld
		| isError m = (liftError m,iworld)
		= write (f v (fromOk m)) iworld
		
mapShared :: !(!r -> r`,!w` r -> w) !(ReadWriteShared r w) -> ReadWriteShared r` w`
mapShared (readMap,writeMap) shared = mapSharedRead readMap (mapSharedWrite writeMap shared)

mapSharedError :: !(!r -> MaybeErrorString r`, !w` r -> MaybeErrorString w)	!(ReadWriteShared r w) -> ReadWriteShared r` w`
mapSharedError (readMap,writeMap) (ReadWriteShared id read write getTimestamp) = ReadWriteShared id nread nwrite getTimestamp
where
	nread iworld = case read iworld of
		(Error e,iworld) = (Error e,iworld)
		(Ok r, iworld) = case readMap r of
			(Error e)	= (Error e,iworld)
			(Ok r`)		= (Ok r`,iworld)

	nwrite v iworld = case read iworld of
		(Error e,iworld) = (Error e,iworld)
		(Ok r, iworld) = case writeMap v r of
			(Error e)	= (Error e,iworld)
			(Ok w`)		= write w` iworld

toReadOnlyShared :: !(ReadWriteShared r w) -> ReadOnlyShared r
toReadOnlyShared (ReadWriteShared id read write getTimestamp) = ReadWriteShared id read (\_ iworld -> (Ok Void,iworld)) getTimestamp

(>+<) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadWriteShared (r0,r1) (w0,w1)
(>+<) (ReadWriteShared id0 read0 write0 getTimestamp0) (ReadWriteShared id1 read1 write1 getTimestamp1)
	= ReadWriteShared (composeIds id0 id1) (composeReads read0 read1) (composeWrites write0 write1) (composeGetTimestamps getTimestamp0 getTimestamp1)

(>+|) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadWriteShared (r0,r1) w0
(>+|) (ReadWriteShared id0 read0 write0 getTimestamp0) (ReadWriteShared id1 read1 _ getTimestamp1)
	= ReadWriteShared (composeIds id0 id1) (composeReads read0 read1) write0 (composeGetTimestamps getTimestamp0 getTimestamp1)

(|+<) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadWriteShared (r0,r1) w1
(|+<) (ReadWriteShared id0 read0 _ getTimestamp0) (ReadWriteShared id1 read1 write1 getTimestamp1)
	= ReadWriteShared (composeIds id0 id1) (composeReads read0 read1) write1 (composeGetTimestamps getTimestamp0 getTimestamp1)
	
(|+|) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadOnlyShared (r0,r1)
(|+|) (ReadWriteShared id0 read0 write0 getTimestamp0) (ReadWriteShared id1 read1 write1 getTimestamp1)
	= ReadWriteShared (composeIds id0 id1) (composeReads read0 read1) (\_ iworld -> (Ok Void,iworld)) (composeGetTimestamps getTimestamp0 getTimestamp1)
	
(>&<) infixl 6 :: !(Shared a) !(Shared b) -> (Shared (a,b)) | gEq{|*|} a & gEq{|*|} b
(>&<) (ReadWriteShared id0 read0 write0 getTimestamp0) (ReadWriteShared id1 read1 write1 getTimestamp1)
	= ReadWriteShared (composeIds id0 id1) (composeReads read0 read1) writeC (composeGetTimestamps getTimestamp0 getTimestamp1)
where
	writeC (na,nb) iworld
		# (oa,iworld)	= read0 iworld
		| isError oa = (liftError oa,iworld)
		# (ob,iworld)	= read1 iworld
		| isError ob = (liftError ob,iworld)
		# (r,iworld)	= if (fromOk oa =!= na) (write0 na iworld) (Ok Void,iworld)
		| isError r = (liftError r,iworld)
		= if (fromOk ob =!= nb) (write1 nb iworld) (Ok Void,iworld)

composeIds id0 id1					= removeDup (id0 ++ id1)
composeReads						= compose (\a b -> (a,b))
composeGetTimestamps				= compose max
composeWrites write0 write1 (v0,v1)	= compose const (write0 v0) (write1 v1)

compose compF f0 f1 iworld
	# (res0,iworld)	= f0 iworld
	| isError res0	= (liftError res0,iworld)
	# (res1,iworld)	= f1 iworld
	| isError res1	= (liftError res1,iworld)
	= (Ok (compF (fromOk res0) (fromOk res1)),iworld)
	
symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a) !(Shared b) -> (!Shared a,!Shared b)
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB 
	newSharedA = mapShared (fst,\a (_,b) -> (a,putr a b)) sharedAll
	newSharedB = mapShared (snd,\b (a,_) -> (putl b a,b)) sharedAll