definition module iTasks.Internal.AsyncSDS

import iTasks.SDS.Definition
from iTasks.WF.Definition import :: TaskId
from iTasks.Internal.IWorld import :: IOState, :: IOStates
from iTasks.Internal.SDS import :: SDSIdentity, :: SDSNotifyRequest

:: SDSRequest r w sds = SDSReadRequest (sds () r w) (Maybe (TaskId, Int)) & TC r & RWShared sds
	| SDSWriteRequest (sds () r w) w & TC r & TC w & RWShared sds
	| SDSModifyRequest (sds () r w) (r -> w) & TC r & TC w & RWShared sds
	| SDSRefreshRequest TaskId SDSIdentity

queueRead :: !(sds p r w) !TaskId Bool !*IWorld -> (!MaybeError TaskException Int, !*IWorld) | TC r & RWShared sds

queueRemoteRefresh :: !SDSIdentity [SDSNotifyRequest] !*IWorld -> *IWorld

queueWrite :: !w !(sds p r w) !TaskId !*IWorld -> (!MaybeError TaskException Int, !*IWorld) | TC r & TC w & RWShared sds

queueModify :: !(r -> w) !(sds p r w) !TaskId !*IWorld -> (!MaybeError TaskException Int, !*IWorld) | TC r & TC w & RWShared sds

getAsyncReadValue :: !(sds p r w) !TaskId !Int IOStates -> Either String (Maybe r) | TC r & RWShared sds

getAsyncWriteValue :: !(sds p r w) !TaskId !Int IOStates -> Either String (Maybe w) | TC w & RWShared sds