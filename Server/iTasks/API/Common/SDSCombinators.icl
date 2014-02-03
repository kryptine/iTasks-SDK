implementation module iTasks.API.Common.SDSCombinators

import StdTuple
import iTasks.API.Core.SDSs, iTasks.API.Core.SDSCombinators
from StdFunc import o, const

mapRead :: !(r -> r`) !(RWShared r w) -> RWShared r` w
mapRead get share = mapReadError (Ok o get) share

mapWrite :: !(w` r -> Maybe w) !(RWShared r w) -> RWShared r w`
mapWrite put share = mapWriteError (\w` r -> Ok (put w` r)) share

mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(RWShared r w) -> RWShared r` w`
mapReadWrite (readMap,writeMap) shared = mapRead readMap (mapWrite writeMap shared)

mapReadError :: !(r -> MaybeErrorString r`) !(RWShared r w) -> RWShared r` w
mapReadError proj share = share >?> \r -> fmap constShare (proj r)

mapWriteError :: !(w` r -> MaybeErrorString (Maybe w)) !(RWShared r w) -> RWShared r w`
mapWriteError proj share = share >!> (const (Ok share),\w` r -> fmap (maybe [] (\w -> [Write w share])) (proj w` r))
	
mapReadWriteError :: !(!r -> MaybeErrorString r`,!w` r -> MaybeErrorString (Maybe w)) !(RWShared r w) -> RWShared r` w`
mapReadWriteError (readMap,writeMap) shared = mapReadError readMap (mapWriteError writeMap shared)

toReadOnly :: !(RWShared r w) -> ROShared r
toReadOnly share = mapWrite (\_ _ -> Nothing) share

(>+<) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) (wx,wy)
(>+<) shareX shareY = (shareX >?> \rx -> Ok (mapRead (\ry -> (rx,ry)) shareY)) >!> (const (Ok (constShare Void)),\(wx,wy) _ -> Ok [Write wx shareX, Write wy shareY])

(>+|) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) wx
(>+|) srcX srcY = mapWrite (\wx _ -> Just (wx, Void)) (srcX >+< toReadOnly srcY)

(|+<) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) wy
(|+<) srcX srcY = mapWrite (\wy _ -> Just (Void, wy)) (toReadOnly srcX >+< srcY)

(|+|) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) Void
(|+|) srcX srcY = toReadOnly (srcX >+< srcY)

(>+>) infixl 6 :: !(RWShared r0 w0) !(r0 -> (RWShared r1 w1)) -> RWShared r1 w1
(>+>) share shareGenF = share >!> (const (Ok share), \w1 r0 -> Ok [Write w1 (shareGenF r0)]) >?> \r0 -> Ok (shareGenF r0)

symmetricLens :: !(a b -> b) !(b a -> a) !(RWShared a a) !(RWShared b b) -> (!RWShared a a, !RWShared b b)
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB 
	newSharedA = mapReadWrite (fst,\a (_,b) -> Just (a,putr a b)) sharedAll
	newSharedB = mapReadWrite (snd,\b (a,_) -> Just (putl b a,b)) sharedAll

