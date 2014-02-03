implementation module iTasks.API.Core.SDSCombinators

import StdTuple
import iTasks.Framework.SDS
import iTasks.API.Core.SDSs
from StdFunc import const, o
import Data.Maybe, Data.Error
	
(>?>) infixl 6 :: !(RWShared rx wx) !(rx -> MaybeErrorString (RWShared ry wy)) -> RWShared ry wx
(>?>) sharex cont = ComposedRead sharex cont

(>!>) infixl 6 :: !(RWShared r w`) !(!w -> MaybeErrorString (RWShared r` w``), !w r` -> MaybeErrorString [WriteShare]) -> RWShared r w
(>!>) share (readOp,writeOp) = ComposedWrite share readOp writeOp
