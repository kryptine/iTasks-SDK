implementation module iTasks.API.Core.SDSCombinators

import StdTuple, StdClass
import iTasks.Framework.SDS, iTasks.Framework.Task
import iTasks.API.Core.SDSs
from StdFunc import const, o
import Data.Maybe, Data.Error

sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(RWShared p rs ws) -> RWShared p r w
sdsProject read write sds = SDSProjection sds {SDSProjection|read=read,write=write}

sdsTranslate :: !String !(p -> ps) !(RWShared ps r w) -> RWShared p r w | TC ps
sdsTranslate name param sds = SDSTranslation sds {SDSTranslation|name=name,param=param}

sdsSplit :: !String !(p -> (ps,pn)) !(pn rs -> r) !(pn rs w -> (ws,SDSNotifyPred pn)) !(RWShared ps rs ws) -> RWShared p r w | TC ps & TC pn & gEq{|*|} ps
sdsSplit name param read write sds = SDSSplit sds {SDSSplit|name=name,param=param,read=read,write=write}

sdsSelect :: !String !(p -> Either p1 p2) !(p1 r w -> SDSNotifyPred p2) !(p2 r w -> SDSNotifyPred p1) !(RWShared p1 r w) !(RWShared p2 r w) -> RWShared p r w | TC p1 & TC p2
sdsSelect name select notifyl notifyr sds1 sds2 = SDSSelect sds1 sds2 {SDSSelect|name=name,select=select,notifyl=notifyl,notifyr=notifyr}

sdsParallel :: !String !(p -> (p1,p2)) !((r1,r2) -> r) !(SDSWriteProjection r1 w1 w) !(SDSWriteProjection r2 w2 w) !(RWShared p1 r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p1 & TC p2
sdsParallel name param read write1 write2 sds1 sds2 = SDSParallel sds1 sds2 {SDSParallel|name=name,param=param,read=read,writel=write1,writer=write2}

sdsSequence :: !String !(p r1 -> p2) !((r1,r2) -> r) !(SDSWriteProjection r1 w1 w) !(SDSWriteProjection r2 w2 w) !(RWShared p r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p2
sdsSequence name param read write1 write2 sds1 sds2 = SDSSequence sds1 sds2 {SDSSequence|name=name,param=param,read=read,writel=write1,writer=write2}
