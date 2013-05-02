implementation module iTasks.Framework.Shared

import Void, Error, StdFunc
import iTasks.Framework.IWorld
from SharedDataSource import :: RWShared, :: Hash, null, ::ROShared, :: WOShared
from SharedDataSource import mapRead, mapWrite, mapReadWrite, mapReadError, mapWriteError, mapReadWriteError, toReadOnly
from SharedDataSource import >+<, >+|, |+<, |+|, createChangeOnWriteSDS, createReadOnlySDS, createReadOnlySDSError
from SharedDataSource import >!>, >?>, :: WriteShare(..)
	
(>+>) infixl 6 :: !(ReadWriteShared r0 w0) !(r0 -> (ReadWriteShared r1 w1)) -> ReadWriteShared r1 w1
(>+>) share shareGenF = share >!> (const (Ok share), \w1 r0 -> Ok [Write w1 (shareGenF r0)]) >?> \r0 -> Ok (shareGenF r0)
	
