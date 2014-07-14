implementation module iTasks.API.Common.SDSCombinators

import StdTuple
import iTasks.API.Core.SDSs, iTasks.API.Core.SDSCombinators
from StdFunc import o, const, flip, id
from iTasks.Framework.Task import exception

sdsFocus :: !p !(RWShared p r w) -> (RWShared p` r w) | TC p & JSONEncode{|*|} p
sdsFocus p sds = sdsTranslate ("("+++ toString (toJSON p)+++")/") (const p) sds

sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(RWShared p rs ws) -> RWShared p r w | TC p
sdsProject read write sds = sdsLens "()" param` read` write` (SDSNotifyConst notify`) sds
where
    param` p = p
    read` = case read of
        SDSLensRead f = SDSRead (\p rs -> f rs)
        SDSConstRead f = SDSReadConst (\_ -> f)

    write` = case write of
        SDSLensWrite f = SDSWrite (\p rs w -> f rs w)
        SDSBlindWrite f = SDSWriteConst (\p w -> f w)
        SDSNoWrite      = SDSWriteConst (\p w -> Ok Nothing)
    notify` p w = const True

sdsTranslate :: !String !(p -> ps) !(RWShared ps r w) -> RWShared p r w | TC ps
sdsTranslate name param sds = sdsLens name param (SDSRead (\_ rs -> Ok rs)) (SDSWriteConst (\_ w -> Ok (Just w))) (SDSNotifyConst (\_ _ _ -> True)) sds

sdsSplit :: !String !(p -> (ps,pn)) !(pn rs -> r) !(pn rs w -> (ws,SDSNotifyPred pn)) !(RWShared ps rs ws) -> RWShared p r w | TC ps & TC pn & gEq{|*|} ps
sdsSplit name param read write sds = sdsLens name param` (SDSRead read`) (SDSWrite write`) (SDSNotify notify`) sds
where
    param` p            = fst (param p)
    read` p rs          = Ok (read (snd (param p)) rs)
    write` p rs w       = Ok (Just (fst (write (snd (param p)) rs w)))
    notify` p rs w pq   = (snd (write (snd (param p)) rs w)) (snd (param pq))

mapRead :: !(r -> r`) !(RWShared p r w) -> RWShared p r` w | TC p
mapRead read sds = mapReadError (\r -> Ok (read r)) sds

mapWrite :: !(w` r -> Maybe w) !(RWShared p r w) -> RWShared p r w` | TC p
mapWrite write sds = mapWriteError (\r w -> Ok (write r w)) sds

mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(RWShared p r w) -> RWShared p r` w` | TC p
mapReadWrite (read,write) sds = mapReadWriteError (\r -> Ok (read r), (\r w -> Ok (write r w))) sds

mapReadError :: !(r -> MaybeError TaskException r`) !(RWShared p r w) -> RWShared p r` w | TC p
mapReadError read sds = sdsProject (SDSLensRead read) (SDSBlindWrite (Ok o Just)) sds

mapWriteError :: !(w` r -> MaybeError TaskException  (Maybe w)) !(RWShared p r w) -> RWShared p r w` | TC p
mapWriteError write sds = sdsProject (SDSLensRead Ok) (SDSLensWrite (flip write)) sds
	
mapReadWriteError :: !(!r -> MaybeError TaskException r`,!w` r -> MaybeError TaskException (Maybe w)) !(RWShared p r w) -> RWShared p r` w` | TC p
mapReadWriteError (read,write) sds = sdsProject (SDSLensRead read) (SDSLensWrite (flip write)) sds

mapSingle :: !(RWShared p [r] [w]) -> (RWShared p r w) | TC p
mapSingle sds = sdsProject (SDSLensRead read) (SDSBlindWrite write) sds
where
    read [x]    = Ok x
    read []     = Error (exception "List element not found")
    read _      = Error (exception "Multiple list elements found, expected only one") //(dynamic notfound,notfound)

    write x     = Ok (Just [x])

    notfound    = "Element not found"

toReadOnly :: !(RWShared p r w) -> ROShared p r | TC p
toReadOnly sds = sdsProject (SDSLensRead Ok) SDSNoWrite sds

(>+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) (wx,wy) | TC p
(>+<) sds1 sds2 = sdsParallel ">+<" (\p -> (p,p)) id (SDSWriteConst write1) (SDSWriteConst write2) sds1 sds2
where
    write1 _ w = Ok (Just (fst w))
    write2 _ w = Ok (Just (snd w))

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

