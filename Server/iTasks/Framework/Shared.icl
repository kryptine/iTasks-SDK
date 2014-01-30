implementation module iTasks.Framework.Shared

from iTasks.Framework.SDS import :: RWShared, :: Hash, null, ::ROShared, :: WOShared
from iTasks.Framework.SDS import mapRead, mapWrite, mapReadWrite, mapReadError, mapWriteError, mapReadWriteError, toReadOnly
from iTasks.Framework.SDS import >+<, >+|, |+<, |+|, createChangeOnWriteSDS, createReadOnlySDS, createReadOnlySDSError
from iTasks.Framework.SDS import >!>, >?>, :: WriteShare(..)
from iTasks.Framework.SDS as SDS import qualified read, write

import iTasks.Framework.IWorld
import iTasks.Framework.Client.Override
from iTasks.Framework.Task import mkInstantTask

import Data.Void, Data.Error, Text.JSON
from Data.Map import qualified put, get

from StdFunc import const
import StdString, StdList, StdClass

(>+>) infixl 6 :: !(ReadWriteShared r0 w0) !(r0 -> (ReadWriteShared r1 w1)) -> ReadWriteShared r1 w1
(>+>) share shareGenF = share >!> (const (Ok share), \w1 r0 -> Ok [Write w1 (shareGenF r0)]) >?> \r0 -> Ok (shareGenF r0)

toJSONShared :: (ReadWriteShared r w) -> Shared JSONNode | JSONEncode{|*|} r & JSONDecode{|*|} w
toJSONShared shared = createChangeOnWriteSDS "exposedShare" "?" read write
where
	read iworld
		# (val,iworld) = 'SDS'.read shared iworld
		= case val of
			(Ok val)  = (Ok (toJSON val), iworld)
			(Error e) = (Error e, iworld)
				
	write json iworld
		= case fromJSON json of
			Nothing
				= (Error "Shared type mismatch in toJSONShared", iworld)
			Just val
				= 'SDS'.write val shared iworld

fromJSONShared :: (Shared JSONNode) -> ReadWriteShared r w | JSONDecode{|*|} r & JSONEncode{|*|} w
fromJSONShared shared = createChangeOnWriteSDS "exposedShare" "?" read write
where
	read iworld
		# (ret,iworld) = 'SDS'.read shared iworld
		= case ret of
			(Ok json)  = case (fromJSON json) of
							(Just val)  = (Ok val, iworld)
							Nothing     = (Error "Shared type mismatch in fromJSONShared", iworld)
			(Error e) = (Error e, iworld)

	write val iworld
		= 'SDS'.write (toJSON val) shared iworld

newSDSId :: !*IWorld -> (!String, !*IWorld)
newSDSId iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld&random = drop 32 random})

newURL :: !*IWorld -> (!String, !*IWorld)
newURL iworld=:{IWorld|server={serverURL},random}
	# (sdsid, iworld) = newSDSId iworld
	= getURLbyId sdsid iworld

// TODO: different URL for clients
getURLbyId :: !String !*IWorld -> (!String, !*IWorld)
getURLbyId sdsid iworld=:{IWorld|server={serverURL},random}
	= ("sds:" +++ serverURL +++ "/" +++ sdsid, iworld)	
