implementation module iTasks.API.Core.SDSCombinators

import StdTuple, StdClass
import iTasks._Framework.SDS, iTasks._Framework.Task
import iTasks.API.Core.SDSs
from StdFunc import const, o
import Data.Maybe, Data.Error

sdsLens :: !String (p -> ps) (SDSLensRead p r rs) (SDSLensWrite p w rs ws) (SDSLensNotify p w rs) !(RWShared ps rs ws) -> RWShared p r w | iTask ps
sdsLens name param read write notify sds = SDSLens sds {SDSLens|name=name,param=param,read=read,write=write,notify=notify}

sdsSelect :: !String !(p -> Either p1 p2) !(p1 r w -> SDSNotifyPred p2) !(p2 r w -> SDSNotifyPred p1) !(RWShared p1 r w) !(RWShared p2 r w) -> RWShared p r w | iTask p1 & iTask p2
sdsSelect name select notifyl notifyr sds1 sds2 = SDSSelect sds1 sds2 {SDSSelect|name=name,select=select,notifyl=notifyl,notifyr=notifyr}

sdsParallel :: !String !(p -> (p1,p2)) !((r1,r2) -> r) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(RWShared p1 r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | iTask p1 & iTask p2
sdsParallel name param read write1 write2 sds1 sds2 = SDSParallel sds1 sds2 {SDSParallel|name=name,param=param,read=read,writel=write1,writer=write2}

sdsSequence :: !String !(p r1 -> p2) !((r1,r2) -> r) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(RWShared p r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | iTask p2
sdsSequence name param read write1 write2 sds1 sds2 = SDSSequence sds1 sds2 {SDSSequence|name=name,param=param,read=read,writel=write1,writer=write2}
