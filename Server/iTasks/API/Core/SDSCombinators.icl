implementation module iTasks.API.Core.SDSCombinators

import StdTuple
import iTasks.Framework.SDS
import iTasks.API.Core.SDSs
from StdFunc import const, o
import Data.Maybe, Data.Error
	
(>?>) infixl 6 :: !(RWShared p rx wx) !(rx -> MaybeErrorString (RWShared p ry wy)) -> RWShared p ry wx
(>?>) sharex cont = ComposedRead sharex cont

(>!>) infixl 6 :: !(RWShared p r w`) !(!w -> MaybeErrorString (RWShared p r` w``), !w r` -> MaybeErrorString [WriteShare p]) -> RWShared p r w
(>!>) share (readOp,writeOp) = ComposedWrite share readOp writeOp

sdsProject :: !(RWShared p rs ws) !(SDSLens rs ws r w) -> RWShared p r w
sdsProject sds lens = SDSProjection sds lens

sdsTranslate :: !(RWShared ps r w) !(p -> ps) -> RWShared p r w | TC ps
sdsTranslate sds translation = SDSTranslation sds translation

sdsSplit :: !(RWShared ps r w) !(SDSSplit p ps pn r w) -> RWShared p r w | TC ps & TC pn
sdsSplit sds split = SDSSplit sds split

sdsMerge :: !(RWShared p1 r w) !(RWShared p2 r w) !(SDSMerge p p1 p2 r w) -> RWShared p r w | TC p1 & TC p2
sdsMerge sds1 sds2 merge = SDSMerge sds1 sds2 merge

sdsParallel :: !(RWShared p1 r1 w1) !(RWShared p2 r2 w2) !(SDSParallel p1 r1 w1 p2 r2 w2 p r w) -> RWShared p r w | TC p1 & TC p2
sdsParallel sds1 sds2 par = SDSParallel sds1 sds2 par

sdsSequence :: !(RWShared p rw1 rw1) !(RWShared p2 r2 w2) !(SDSSequence rw1 p2 r2 w2 r w) -> RWShared p r w | TC p2
sdsSequence sds1 sds2 seq = SDSSequence sds1 sds2 seq
