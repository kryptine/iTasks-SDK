definition module iTasks.Extensions.Distributed.Definition

import iTasks
import iTasks.Extensions.User

import qualified Data.Map

derive class iTask DomainTask, DomainDevice, DomainTaskState, Claimer, ClaimStatus
derive JSONEncode TaskResult, TaskEvalInfo
derive JSONDecode TaskResult, TaskEvalInfo

:: SerializedTask :== String
:: SerializedTaskResult :== String

// Valid across multiple devices in a single domain.
:: DistributedTaskId :== Int

:: DomainTask = DomainTask TaskAttributes SerializedTask

:: Claimer = User User DateTime
:: ClaimStatus = NoClaim | Claimed Claimer

:: DTaskMap :== Map DistributedTaskId (DomainTask, ClaimStatus, SerializedTaskResult)

:: DomainTaskState = { nextTaskId :: DistributedTaskId
	, tasks :: DTaskMap}

:: DomainDevice = DomainDevice Hostname Port

appTasks :: (DTaskMap -> DTaskMap) DomainTaskState -> DomainTaskState