implementation module iTasks.API.Core.SDSCombinators

import StdTuple, StdClass
import iTasks.Framework.SDS, iTasks.Framework.Task
import iTasks.API.Core.SDSs
from StdFunc import const, o
import Data.Maybe, Data.Error
	
(>?>) infixl 6 :: !(RWShared p rx wx) !(rx -> MaybeError TaskException (RWShared p ry wy)) -> RWShared p ry wx
(>?>) sharex cont = ComposedRead sharex cont

(>!>) infixl 6 :: !(RWShared p r w`) !(!w -> MaybeError TaskException (RWShared p r` w``), !w r` -> MaybeError TaskException [WriteShare p]) -> RWShared p r w
(>!>) share (readOp,writeOp) = ComposedWrite share readOp writeOp

sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(RWShared p rs ws) -> RWShared p r w
sdsProject read write sds = SDSProjection sds {SDSProjection|read=read,write=write}

sdsTranslate :: !(p -> ps) !(RWShared ps r w) -> RWShared p r w | TC ps
sdsTranslate translation sds = SDSTranslation sds translation

sdsFocus :: !p !(RWShared p r w) -> (RWShared p` r w) | TC p
sdsFocus p sds = sdsTranslate (const p) sds

sdsSplit :: !(p -> (ps,pn)) !(pn rs -> r) !(pn rs w -> (ws,SDSNotifyPred pn)) !(RWShared ps rs ws) -> RWShared p r w | TC ps & TC pn & gEq{|*|} ps
sdsSplit param read write sds = SDSSplit sds {SDSSplit|param=param,read=read,write=write}

sdsMerge :: !(p -> Either p1 p2) !(p1 r w -> SDSNotifyPred p2) !(p2 r w -> SDSNotifyPred p1) !(RWShared p1 r w) !(RWShared p2 r w) -> RWShared p r w | TC p1 & TC p2
sdsMerge select notifyl notifyr sds1 sds2 = SDSMerge sds1 sds2 {SDSMerge|select=select,notifyl=notifyl,notifyr=notifyr}

sdsParallel :: !(p -> (p1,p2)) !((r1,r2) -> r) !(SDSWriteProjection r1 w1 w) !(SDSWriteProjection r2 w2 w) !(RWShared p1 r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p1 & TC p2
sdsParallel param read write1 write2 sds1 sds2 = SDSParallel sds1 sds2 {SDSParallel|param=param,read=read,writel=write1,writer=write2}

sdsSequence :: !(p r1 -> p2) !((r1,r2) -> r) !(SDSWriteProjection r1 w1 w) !(SDSWriteProjection r2 w2 w) !(RWShared p r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p2
sdsSequence param read write1 write2 sds1 sds2 = SDSSequence sds1 sds2 {SDSSequence|param=param,read=read,writel=write1,writer=write2}
