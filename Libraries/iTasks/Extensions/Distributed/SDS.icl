implementation module iTasks.Extensions.Distributed.SDS

import iTasks
import Data.Func, Data.Either
import qualified Data.Map

import iTasks.Extensions.Distributed.Definition
import iTasks.Internal.Distributed.Formatter

domainName :: SDSLens () (Maybe Domain) (Maybe Domain)
domainName = sharedStore "domain" Nothing

domainUsers :: Domain -> SDSSequence () [User] [User]
domainUsers domain
= maybeDomainShare domain "users" (sharedStore "domainUsers" [])

domainTasks :: Domain -> SDSSequence () DomainTaskState DomainTaskState
domainTasks domain
= maybeDomainShare domain "tasks" (sharedStore "domainTasks" {nextTaskId = 0, tasks = 'Data.Map'.newMap})

domainTask :: Domain DistributedTaskId -> SDSLens () (Maybe (DomainTaskReference, SerializedTaskResult)) ()
domainTask domain dTaskId = mapRead read (toReadOnly $ domainTasks domain)
where
	read {tasks} = 'Data.Map'.get dTaskId tasks

domainTaskValue :: Domain DistributedTaskId {#Symbol} -> SDSLens () (TaskValue a) () | iTask a
domainTaskValue domain dTaskId symbols = mapRead read (domainTask domain dTaskId)
where
	read Nothing = NoValue
	read (Just (_, result)) = deserializeFromBase64 result symbols

domainDevices :: Domain -> SDSSequence () [DomainDevice] [DomainDevice]
domainDevices domain
= maybeDomainShare domain "devices" (sharedStore "domainDevices" [])

// Selects to use a remote share when the current instance is not the host in the given domain. It then queries the host of the domain.
maybeDomainShare :: Domain String (sds () r w) -> (SDSSequence () r w) | iTask r & iTask w & RWShared sds
maybeDomainShare (Domain host port) name sds = sdsSequence ("seq" +++ name)
	(const ()) (\p mbDomain. mbDomain) (\p mbDomain. Right snd)
	(SDSWriteConst \p w. Ok Nothing)
	(SDSWrite \p rs w. Ok (Just w))
	domainName
	select
where
	select = sdsSelect ("select" +++ name) selectp
		(SDSNotify \pw rs w ts p. True)
		(SDSNotify \pw rs w ts p. True)
		sds
		(remoteShare sds {SDSShareOptions|domain=host, port=port})

	selectp Nothing = Left ()
	selectp (Just domain)
	| domain == (Domain host port) = Left ()
	= Right ()


/**
 * Creates a share which observes the result of the given domain task.
 * TODO: Fix
 */
domainTaskResult :: DomainTaskReference -> SDSLens () (TaskResult a) () | iTask a
domainTaskResult ref = toReadOnly (sharedStore "domainTaskResult" DestroyedResult)