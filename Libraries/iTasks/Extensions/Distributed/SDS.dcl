definition module iTasks.Extensions.Distributed.SDS

import iTasks

import iTasks.Extensions.Distributed.Definition
import iTasks.Internal.Distributed.Symbols

/**
 * Holds the domain for which the current instance is a server. Ensures that
 * the same share definitions can be used on servers and clients.
 *
 * For instance, when creating a domainTaskShare with domain "TEST:9999",
 * that share will refer to local shares when the domain has been set to
 * "TEST:9999" through this share.
 */
domainName :: SDSLens () (Maybe Domain) (Maybe Domain)

/**
 * Users in a domain.
 */
domainUsers :: Domain -> SDSSequence () [User] [User]

/**
 * Distributed tasks in a domain.
 */
domainTasks :: Domain -> SDSSequence () DomainTaskState DomainTaskState

/**
 * A share for a single distributed task. It could be the case that the
 * reference is no longer valid (task has been removed/completed by somebody
 * else). In that case, reading from the share will result in Nothing.
 */
domainTask :: Domain DistributedTaskId -> SDSLens () (Maybe (DomainTask, SerializedTaskResult)) ()

/**
 * Share denoting the value of a task in the given domain. When the reference
 * is no longer valid, returns NoValue.
 */
domainTaskValue :: Domain DistributedTaskId {#Symbol} -> SDSLens () (TaskValue a) () | iTask a

/**
 * List of devices in a domain
 */
domainDevices :: Domain -> SDSSequence () [DomainDevice] [DomainDevice]

/**
 * Creates a task which observes the result of the given domain task.
 */
domainTaskResult :: DistributedTaskId -> SDSLens () (TaskResult a) () | iTask a
