implementation module iTasks.Framework.Shared

from Data.SharedDataSource import :: RWShared, :: Hash, null, ::ROShared, :: WOShared
from Data.SharedDataSource import mapRead, mapWrite, mapReadWrite, mapReadError, mapWriteError, mapReadWriteError, toReadOnly
from Data.SharedDataSource import >+<, >+|, |+<, |+|, createChangeOnWriteSDS, createReadOnlySDS, createReadOnlySDSError
from Data.SharedDataSource import >!>, >?>, :: WriteShare(..)

import iTasks.Framework.IWorld
import iTasks.Framework.Client.Override
from iTasks.Framework.Task import mkInstantTask

import Data.Void, Data.Error
from Data.Map import qualified put, get

from StdFunc import const
import StdString, StdList, StdClass

(>+>) infixl 6 :: !(ReadWriteShared r0 w0) !(r0 -> (ReadWriteShared r1 w1)) -> ReadWriteShared r1 w1
(>+>) share shareGenF = share >!> (const (Ok share), \w1 r0 -> Ok [Write w1 (shareGenF r0)]) >?> \r0 -> Ok (shareGenF r0)

newSDSId :: !*IWorld -> (!String, !*IWorld)
newSDSId iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld&random = drop 32 random})

// TODO: different URL for clients
genURLforShared :: !*IWorld -> (!String, !*IWorld)
genURLforShared iworld=:{IWorld|serverURL,random}
	# (sdsid, iworld) = newSDSId iworld
	= ("sds:" +++ serverURL +++ "/" +++ sdsid, iworld)	
	 				
	 				
	 				