implementation module iTasks.API.Common.SDSCombinators

import StdTuple
import iTasks.API.Core.SDSs, iTasks.API.Core.SDSCombinators
from StdFunc import o, const, flip, id
from iTasks.Framework.Task import exception

sdsFocus :: !p !(RWShared p r w) -> (RWShared p` r w) | TC p & JSONEncode{|*|} p
sdsFocus p sds = sdsTranslate ("("+++ toString (toJSON p)+++")/") (const p) sds

mapRead :: !(r -> r`) !(RWShared p r w) -> RWShared p r` w
mapRead read sds = mapReadError (\r -> Ok (read r)) sds

mapWrite :: !(w` r -> Maybe w) !(RWShared p r w) -> RWShared p r w`
mapWrite write sds = mapWriteError (\r w -> Ok (write r w)) sds

mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(RWShared p r w) -> RWShared p r` w`
mapReadWrite (read,write) sds = mapReadWriteError (\r -> Ok (read r), (\r w -> Ok (write r w))) sds

mapReadError :: !(r -> MaybeError TaskException r`) !(RWShared p r w) -> RWShared p r` w
mapReadError read sds = sdsProject (SDSLensRead read) (SDSBlindWrite (Ok o Just)) sds

mapWriteError :: !(w` r -> MaybeError TaskException  (Maybe w)) !(RWShared p r w) -> RWShared p r w`
mapWriteError write sds = sdsProject (SDSLensRead Ok) (SDSLensWrite (flip write)) sds
	
mapReadWriteError :: !(!r -> MaybeError TaskException r`,!w` r -> MaybeError TaskException (Maybe w)) !(RWShared p r w) -> RWShared p r` w`
mapReadWriteError (read,write) sds = sdsProject (SDSLensRead read) (SDSLensWrite (flip write)) sds

mapSingle :: !(RWShared p [r] [w]) -> (RWShared p r w)
mapSingle sds = sdsProject (SDSLensRead read) (SDSBlindWrite write) sds
where
    read [x]    = Ok x
    read []     = Error (exception "List element not found")
    read _      = Error (exception "Multiple list elements found, expected only one") //(dynamic notfound,notfound)

    write x     = Ok (Just [x])

    notfound    = "Element not found"

toReadOnly :: !(RWShared p r w) -> ROShared p r
toReadOnly sds = sdsProject (SDSLensRead Ok) SDSNoWrite sds

(>+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) (wx,wy) | TC p
(>+<) sds1 sds2 = sdsParallel ">+<" (\p -> (p,p)) id (SDSBlindWrite (Ok o Just o fst)) (SDSBlindWrite (Ok o Just o snd)) sds1 sds2

(>+|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wx | TC p
(>+|) srcX srcY = mapWrite (\wx _ -> Just (wx, Void)) (srcX >+< toReadOnly srcY)

(|+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wy | TC p
(|+<) srcX srcY = mapWrite (\wy _ -> Just (Void, wy)) (toReadOnly srcX >+< srcY)

(|+|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) Void | TC p
(|+|) srcX srcY = toReadOnly (srcX >+< srcY)

symmetricLens :: !(a b -> b) !(b a -> a) !(RWShared p a a) !(RWShared p b b) -> (!RWShared p a a, !RWShared p b b) | TC p
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB
	newSharedA = mapReadWrite (fst,\a (_,b) -> Just (a,putr a b)) sharedAll
	newSharedB = mapReadWrite (snd,\b (a,_) -> Just (putl b a,b)) sharedAll

