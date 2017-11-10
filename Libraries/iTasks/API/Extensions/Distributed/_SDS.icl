implementation module iTasks.API.Extensions.Distributed._SDS

import iTasks.API.Extensions.Distributed._Util
import iTasks.API.Extensions.Distributed._Types
import iTasks.API.Extensions.Distributed._Formatter
import iTasks.API.Extensions.Distributed.RemoteTask
import iTasks.API.Extensions.Distributed.Instance
import iTasks.API.Extensions.Distributed._Attributes
import qualified Data.Map as DM
import Text.Encodings.Base64
import iTasks.API.Extensions.Distributed.Engine

from iTasks.Internal.Serialization import dynamicJSONEncode, dynamicJSONDecode
from iTasks.UI.Editor.Common import emptyEditor

rr_get :: !(ReadWriteShared a w) -> Task a | iTask a & iTask w
rr_get shared
	= get currentTaskInstanceNo
	>>= \taskid -> currentDistributedId
	>>= \instanceId -> if (isLocal instanceId)
		(get shared)
		(remoteGet taskid instanceId shared)
where 
	remoteGet taskid sharedid shared
		# myRef = (toString taskid)
		# ref = (toString sharedid)
		# tempShared = dummyShared myRef Nothing
		= addSharedHandler taskid (putSharedValueInDummyShared tempShared)
		>>| sendRequestToShareHost sharedid (message ref myRef shared)
		>>| watch tempShared >>* [OnValue (ifValue (\v -> not (isNothing v)) \v -> (return (fromJust v)))]
	message ref myRef shared = "share " +++ ref +++ " " +++ myRef +++ " get " +++ (serializeToBase64 (Remote_Share shared))

rr_upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
rr_upd func share
	= get currentTaskInstanceNo
	>>= \taskid -> currentDistributedId
        >>= \instanceId -> if (isLocal instanceId)
		(upd func share)
		(remoteUpd func taskid instanceId share)
where
	remoteUpd func taskid sharedid shared
		# myRef = (toString taskid)
		# ref = (toString sharedid)
		# tempShared = dummyShared myRef Nothing
		= addSharedHandler taskid (putSharedValueInDummyShared tempShared)
		>>| sendRequestToShareHost sharedid (message ref myRef shared)
		>>| watch tempShared >>* [OnValue (ifValue (\v -> not (isNothing v)) \v -> (return (fromJust v)))]
	message ref myRef shared = "share " +++ ref +++ " " +++ myRef +++ " upd " +++ (serializeToBase64 (Remote_Share share) +++ " function " +++ (serializeToBase64 func))

rr_set :: !a !(ReadWriteShared r a)  -> Task a | iTask a & iTask r
rr_set value shared = rr_upd (update value) shared
where
        update :: a r -> a | iTask a & iTask r
        update value _ = value

rr_watch :: !(ReadWriteShared r w) -> Task r | iTask r & iTask w
rr_watch shared
	= get currentTaskInstanceNo
	>>- \taskid -> currentDistributedId
	>>- \instanceNo -> if (isLocal instanceNo)
		(watch shared)
		(remoteWatch taskid instanceNo shared)
where
	remoteWatch taskid sharedid shared
		# myRef = (toString taskid) +++ "watch"
		# ref = (toString sharedid)
		# tempShared = dummyShared myRef Nothing
		= set Nothing tempShared
		>>| addSharedHandlerW myRef (putSharedValueInDummyShared tempShared)
		>>| sendRequestToShareHost sharedid (message ref myRef shared (const True))
		>>| watch tempShared >>* [OnValue (ifValue (\v -> isJust v) \v -> (return (fromJust v)))]
		>>- \val -> let tempWatchShared = dummyShared (myRef +++ "__2") val in
			set val tempWatchShared >>| (watch tempWatchShared) -|| (loop val tempWatchShared tempShared taskid sharedid shared)
		
	message ref myRef shared notifyfunc = "share " +++ ref +++ " " +++ myRef +++ " notify " +++ (serializeToBase64 (Remote_Share shared)) +++ " function " +++ (serializeToBase64 notifyfunc)

	loop initialValue watchShared tempShared taskid sharedid shared
		= setValue initialValue tempShared taskid sharedid shared
		>>| forever (repeat watchShared tempShared taskid sharedid shared)
	where
		repeat watchShared tempShared taskid sharedid shared
			= watch tempShared >>* [OnValue (ifValue isJust \v -> (set (fromJust v) watchShared))]
			>>- \value -> setValue value tempShared taskid sharedid shared

		setValue value tempShared taskid sharedid shared
	                # myRef = (toString taskid) +++ "watch"
	                # ref = (toString sharedid) 
			= set Nothing tempShared
			>>| sendRequestToShareHost sharedid (message ref myRef shared ((=!=) value))
			>>| return value
			
:: RemoteShareHandelers :== Map String Handler

:: Handler :== (String -> Task())

gText{|Handler|} _ _             = []
gEditor{|Handler|}               = emptyEditor
JSONEncode{|Handler|} _ c        = [dynamicJSONEncode c]
JSONDecode{|Handler|} _ [c:r]    = (dynamicJSONDecode c,r)
JSONDecode{|Handler|} _ r        = (Nothing,r)
gEq{|Handler|} _ _               = True
gDefault{|Handler|}              = \_ -> return ()

remoteSharedHandelersShared :: Shared RemoteShareHandelers
remoteSharedHandelersShared = memoryShare_ "remote_shared_handelers" 'DM'.newMap

addSharedHandler :: Int (String -> Task ()) -> Task ()
addSharedHandler taskid handlerTask
	= upd (\handlers -> 'DM'.put (toString taskid) handlerTask handlers) remoteSharedHandelersShared @! ()

addSharedHandlerW :: String (String -> Task ()) -> Task ()
addSharedHandlerW ref handlerTask
	= upd (\handlers -> 'DM'.put ref handlerTask handlers) remoteSharedHandelersShared @! ()

callSharedHandler :: String String -> Task ()
callSharedHandler ref data
	= get remoteSharedHandelersShared
	>>= \handlers -> (fromMaybe (\_ -> return ()) ('DM'.get ref handlers)) data

dummyShared :: String a -> RWShared () a a | iTask a
dummyShared ref default = memoryShare_ ("dummy_share_" +++ ref) default

putSharedValueInDummyShared :: (Shared (Maybe a)) String -> Task () | iTask a
putSharedValueInDummyShared shared value
        = let v = (fromJSON (fromString (base64Decode value))) in
                if (not (isNothing v)) ((set v shared) @! ()) (return ())

shareOperation :: String String [String] (String -> Task ()) -> Task ()
shareOperation ref refClient request send = withSymbols (shareOperation` ref refClient request send)

shareOperation` :: String String [String] (String -> Task ()) {#Symbol} -> Task ()
shareOperation` ref refClient ["get", share] send symbols
        = case deserializeFromBase64 share symbols of
                (Remote_Share share)
                        -> get (share) >>= \v -> shareResponse ref refClient v send @! ()
shareOperation` ref refClient ["upd", share, "function", function] send symbols
        # func = deserializeFromBase64 function symbols 
	= case deserializeFromBase64 share symbols of
                (Remote_Share share)
                        -> upd func share >>= \v -> shareResponse ref refClient v send @! ()
shareOperation` ref refClient ["notify", share, "function", function] send symbols
	# func = deserializeFromBase64 function symbols
	= case deserializeFromBase64 share symbols of
                (Remote_Share share)
                        -> appendTopLevelTask ('DM'.fromList []) True (notifyShare ref refClient share func send) @! ()
shareOperation` ref refClient ["value", value] _ _
	= callSharedHandler refClient value

notifyShare :: String String !(ReadWriteShared r w) (r -> Bool) (String -> Task ()) -> Task () | iTask r & iTask w
notifyShare ref refClient share func send
        = (watch share >>* [OnValue (ifValue func (\val -> shareResponse ref refClient val send))]) @! ()

shareResponse :: String String a (String -> Task ()) -> Task () | iTask a
shareResponse ref refClient v send
        # value = ("share " +++ ref +++ " " +++ refClient +++ " value " +++ (base64Encode (toString (toJSON v))))
        = send value

sendRequestToShareHost :: InstanceReference String -> Task ()
sendRequestToShareHost (DistributedInstancePool _ serverId) request = sendRequestToInstanceServer serverId request
