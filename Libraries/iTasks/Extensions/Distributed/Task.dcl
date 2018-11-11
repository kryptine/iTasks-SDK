definition module iTasks.Extensions.Distributed.Task

import iTasks

import iTasks.Extensions.Distributed.API

/**
 * Appends a task to a domain. Yields a reference to the task as appended in the domain.
 */
appendDomainTask :: Domain TaskAttributes (Task a) -> Task DistributedTaskId | iTask a

/**
 * Queues a task in a domain and returns only when there is a result for the task.
 */
queueDomainTask :: Domain TaskAttributes (Task a) -> Task a | iTask a

/**
 * Claims a task in a domain and immediately execute it.
 */
executeDomainTask :: Domain DistributedTaskId -> Task ()

/**
 * Claim a task in the domain an put it into the local task queue
 */
claimDomainTask :: Domain DistributedTaskId -> Task ()

/**
 * Creates a task which denotes whether the given task was removed.
 * Returns True when the task was removed.
 * Returns False when the task could not be found.
 */
removeDomainTask :: Domain DistributedTaskId -> Task ()