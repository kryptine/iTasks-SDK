definition module iTasks.Internal.AsyncSDS

import iTasks.SDS.Definition
from iTasks.WF.Definition import :: TaskId
from iTasks.Internal.IWorld import :: IOState, :: IOStates
from iTasks.Internal.SDS import :: SDSIdentity, :: SDSNotifyRequest

:: SDSRequest p r w sds = SDSReadRequest (sds p r w) (Maybe (TaskId, Int)) & TC r & Readable sds
	| SDSWriteRequest (sds p r w) w & TC r & TC w & Writable sds
	| SDSModifyRequest (sds p r w) (r -> w) & TC r & TC w & Readable, Writable sds
	| SDSRefreshRequest TaskId SDSIdentity

queueRead :: !(SDSRemoteSource p r w) !TaskId Bool !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | TC r

queueServiceRequest :: !(SDSRemoteService p r w) !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | TC r

queueRemoteRefresh :: !SDSIdentity [SDSNotifyRequest] !*IWorld -> *IWorld

queueWrite :: !w !(SDSRemoteSource p r w) !TaskId !*IWorld -> (!MaybeError TaskException Int, !*IWorld) | TC r & TC w

queueModify :: !(r -> w) !(SDSRemoteSource p r w) !TaskId !*IWorld -> (!MaybeError TaskException Int, !*IWorld) | TC r & TC w

getAsyncReadValue :: !(sds p r w) !TaskId !Int IOStates -> Either String (Maybe r) | TC r

getAsyncWriteValue :: !(sds p r w) !TaskId !Int IOStates -> Either String (Maybe w) | TC w