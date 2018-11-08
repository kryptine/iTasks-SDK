definition module iTasks.Extensions.Distributed.Definition

import iTasks

import qualified Data.Map

:: SerializedTask :== String
:: SerializedTaskResult :== String

// Valid across multiple devices in a single domain.
:: DistributedTaskId :== Int

:: DomainTaskReference = DomainTaskReference TaskAttributes SerializedTask
:: DTaskMap :== Map DistributedTaskId (DomainTaskReference, SerializedTaskResult)

:: DomainTaskState = { nextTaskId :: DistributedTaskId
	, tasks :: DTaskMap}

appTasks :: (DTaskMap -> DTaskMap) DomainTaskState -> DomainTaskState

:: DomainDevice = DomainDevice Hostname Port

:: DomainTask = E. a: DomainTask (DomainTaskReference, Task a) & iTask a

derive class iTask DomainTaskReference, DomainDevice, DomainTaskState
derive JSONEncode TaskResult, TaskEvalInfo
derive JSONDecode TaskResult, TaskEvalInfo