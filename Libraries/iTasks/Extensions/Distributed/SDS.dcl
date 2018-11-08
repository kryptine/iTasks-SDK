definition module iTasks.Extensions.Distributed.SDS

import iTasks

import iTasks.Extensions.Distributed.Definition
import iTasks.Internal.Distributed.Symbols

domainName :: SDSLens () (Maybe Domain) (Maybe Domain)

domainUsers :: Domain -> SDSSequence () [User] [User]

domainTasks :: Domain -> SDSSequence () DomainTaskState DomainTaskState

domainTask :: Domain DistributedTaskId -> SDSLens () (Maybe (DomainTaskReference, SerializedTaskResult)) ()

domainTaskValue :: Domain DistributedTaskId {#Symbol} -> SDSLens () (TaskValue a) () | iTask a

domainDevices :: Domain -> SDSSequence () [DomainDevice] [DomainDevice]

/**
 * Creates a task which observes the result of the given domain task.
 */
domainTaskResult :: DomainTaskReference -> SDSLens () (TaskResult a) () | iTask a
