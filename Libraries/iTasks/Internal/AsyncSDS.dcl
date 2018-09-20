definition module iTasks.Internal.AsyncSDS

import iTasks.SDS.Definition
from iTasks.WF.Definition import :: TaskId
from iTasks.Internal.IWorld import :: IOState, :: IOStates
from iTasks.Internal.SDS import :: SDSIdentity, :: SDSNotifyRequest

:: SDSRequest p r w = E. sds: SDSReadRequest !(sds p r w) p & gText{|*|} p & TC p & TC r & TC w & Readable sds
	/*
	 * sds: SDS to read
	 * p: parameter with which to read the SDS
	 * reqSDSId: id of the original SDS read
	 * remoteSDSId: id of the current remote sds, so that refresh events can be sent using this identity.
	 * taskId: taskId of the task on the current instance
	 * port: Port which to send refresh messages on
	 */
	| E. sds: SDSRegisterRequest !(sds p r w) !p !SDSIdentity !SDSIdentity !TaskId !Int & gText{|*|} p & TC p & TC r & TC w & Registrable sds & Readable sds
	| E. sds: SDSWriteRequest !(sds p r w) !p !w & gText{|*|} p & TC p & TC r & TC w & Writeable sds
	| E. sds: SDSModifyRequest !(sds p r w) !p (r -> MaybeError TaskException w) & gText{|*|} p & TC p & TC r & TC w & Modifiable sds
	| SDSRefreshRequest TaskId SDSIdentity

queueRead :: !(SDSRemoteSource p r w) p !TaskId !Bool !SDSIdentity !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w

queueServiceRequest :: !(SDSRemoteService p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r

queueRemoteRefresh :: ![SDSNotifyRequest] !*IWorld -> *IWorld

queueWrite :: !w !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w

queueModify :: !(r -> MaybeError TaskException w) !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w

getAsyncReadValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> Either String (Maybe r) | TC r

getAsyncWriteValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> Either String (Maybe w) | TC w

getAsyncModifyValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> Either String (Maybe (r,w)) | TC w & TC r