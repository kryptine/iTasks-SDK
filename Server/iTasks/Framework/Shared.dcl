definition module iTasks.Framework.Shared

import Data.Void, Data.Error
import iTasks.Framework.IWorld
from Data.SharedDataSource import :: RWShared, null, ::ROShared, :: WOShared
from Data.SharedDataSource import mapRead, mapWrite, mapReadWrite, mapReadError, mapWriteError, mapReadWriteError, toReadOnly, >+<, >+|, |+<, |+|
from Data.SharedDataSource import createChangeOnWriteSDS, createPollingSDS, createReadOnlySDS, createReadOnlySDSError, createReadOnlySDSPredictable, createReadOnlySDSErrorPredictable

:: ReadWriteShared r w	:== RWShared r w IWorld
:: Shared a				:== ReadWriteShared a a
:: ReadOnlyShared a		:== ReadWriteShared a Void
:: WriteOnlyShared a	:== ReadWriteShared Void a
	
// Use the value of one share as parameter for another
(>+>) infixl 6 :: !(ReadWriteShared r0 w0) !(r0 -> (ReadWriteShared r1 w1)) -> ReadWriteShared r1 w1
