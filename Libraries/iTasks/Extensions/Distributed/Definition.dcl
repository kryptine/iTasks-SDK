definition module iTasks.Extensions.Distributed.Definition

import iTasks

import qualified Data.Map

derive class iTask DomainTask, DomainDevice, DomainTaskState
derive JSONEncode TaskResult, TaskEvalInfo
derive JSONDecode TaskResult, TaskEvalInfo

:: SerializedTask :== String
:: SerializedTaskResult :== String

// Valid across multiple devices in a single domain.
:: DistributedTaskId :== Int

:: DomainTask = DomainTask TaskAttributes SerializedTask
:: DTaskMap :== Map DistributedTaskId (DomainTask, SerializedTaskResult)

:: DomainTaskState = { nextTaskId :: DistributedTaskId
	, tasks :: DTaskMap}

:: DomainDevice = DomainDevice Hostname Port

appTasks :: (DTaskMap -> DTaskMap) DomainTaskState -> DomainTaskState