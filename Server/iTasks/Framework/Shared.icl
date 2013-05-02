implementation module iTasks.Framework.Shared

import Data.Void, Data.Error, StdFunc
import iTasks.Framework.IWorld
from Data.SharedDataSource import :: RWShared, :: Hash, null, ::ROShared, :: WOShared
from Data.SharedDataSource import mapRead, mapWrite, mapReadWrite, mapReadError, mapWriteError, mapReadWriteError, toReadOnly
from Data.SharedDataSource import >+<, >+|, |+<, |+|, createChangeOnWriteSDS, createReadOnlySDS, createReadOnlySDSError
from Data.SharedDataSource import >!>, >?>, :: WriteShare(..)
	
(>+>) infixl 6 :: !(ReadWriteShared r0 w0) !(r0 -> (ReadWriteShared r1 w1)) -> ReadWriteShared r1 w1
(>+>) share shareGenF = share >!> (const (Ok share), \w1 r0 -> Ok [Write w1 (shareGenF r0)]) >?> \r0 -> Ok (shareGenF r0)
	
