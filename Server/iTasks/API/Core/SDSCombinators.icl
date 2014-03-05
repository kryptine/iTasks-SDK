implementation module iTasks.API.Core.SDSCombinators

import StdTuple, StdClass
import iTasks.Framework.SDS
import iTasks.API.Core.SDSs
from StdFunc import const, o
import Data.Maybe, Data.Error
	
(>?>) infixl 6 :: !(RWShared p rx wx) !(rx -> MaybeErrorString (RWShared p ry wy)) -> RWShared p ry wx
(>?>) sharex cont = ComposedRead sharex cont

(>!>) infixl 6 :: !(RWShared p r w`) !(!w -> MaybeErrorString (RWShared p r` w``), !w r` -> MaybeErrorString [WriteShare p]) -> RWShared p r w
(>!>) share (readOp,writeOp) = ComposedWrite share readOp writeOp

sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(RWShared p rs ws) -> RWShared p r w
sdsProject read write sds = SDSProjection sds {SDSProjection|read=read,write=write}

sdsTranslate :: !(p -> ps) !(RWShared ps r w) -> RWShared p r w | TC ps
sdsTranslate translation sds = SDSTranslation sds translation

sdsSplit :: !(p -> (ps,pn)) !(pn r -> r) !(pn r w -> (w,SDSNotifyPred pn)) !(RWShared ps r w) -> RWShared p r w | TC ps & TC pn & Eq ps
sdsSplit param read write sds = SDSSplit sds {SDSSplit|param=param,read=read,write=write}

sdsMerge :: !(p -> Either p1 p2) !(p1 r w -> SDSNotifyPred p2) !(p2 r w -> SDSNotifyPred p1) !(RWShared p1 r w) !(RWShared p2 r w) -> RWShared p r w | TC p1 & TC p2
sdsMerge select notifyl notifyr sds1 sds2 = SDSMerge sds1 sds2 {SDSMerge|select=select,notifyl=notifyl,notifyr=notifyr}

sdsParallel :: !(p -> (p1,p2)) !((r1,r2) -> r) !(w -> (w1,w2)) !(RWShared p1 r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p1 & TC p2
sdsParallel param read write sds1 sds2 = SDSParallel sds1 sds2 {SDSParallel|param=param,read=read,write=write}

sdsSequence :: !(r1 -> p2) !((r1,r2) -> r) !(SDSWriteProjection r1 w1 w) !(SDSWriteProjection r2 w2 w) !(RWShared p r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p2
sdsSequence param read writel writer sds1 sds2 = SDSSequence sds1 sds2 {SDSSequence|param=param,read=read,writel=writel,writer=writer}
