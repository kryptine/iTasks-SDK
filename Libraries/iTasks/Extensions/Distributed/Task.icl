implementation module iTasks.Extensions.Distributed.Task

import iTasks.Extensions.Distributed.Definition
import iTasks.Extensions.Distributed.SDS
import iTasks.Internal.Distributed.Formatter
import iTasks.Internal.Distributed.Symbols

import qualified Data.Map

appendDomainTask :: Domain TaskAttributes (Task a) -> Task DistributedTaskId | iTask a
appendDomainTask domain attributes task
= upd appendT (domainTasks domain)
		>>= \{nextTaskId}. (return (nextTaskId-1))
where
	appendT {nextTaskId, tasks} = {nextTaskId = nextTaskId + 1
		, tasks = 'Data.Map'.put nextTaskId (DomainTask attributes serializedTask, serializeToBase64 NoValue) tasks}

	serializedTask = serializeToBase64 task

queueDomainTask :: Domain TaskAttributes (Task a) -> Task a | iTask a
queueDomainTask domain attr task = appendDomainTask domain attr task
	>>= \dTaskId. withSymbols
		\symbols. watch (domainTaskValue domain dTaskId symbols)
			>>* [OnValue ifDoubleStable]
where
	ifDoubleStable (Value (Value v True) _) = Just v
	ifDoubleStable _ = Nothing

claimDomainTask :: Domain DistributedTaskId -> Task ()
claimDomainTask domain dTaskId = get (domainTask domain dTaskId)
 	>>= \mbTask. case mbTask of
 		Nothing = throw ("no task with given identifier: " +++ toString dTaskId)
 		Just (DomainTask attributes taskF, result)
 		= upd (appTasks ('Data.Map'.del dTaskId)) (domainTasks domain)
 			>>= \_. return ()

executeDomainTask :: Domain DistributedTaskId -> Task ()
executeDomainTask domain dTaskId = get (domainTask domain dTaskId)
	>>= \mbTask. case mbTask of
		Nothing = throw ("no task with given identifier: " +++ toString dTaskId)
		Just (DomainTask attributes taskF, result)
		= upd (appTasks ('Data.Map'.del dTaskId)) (domainTasks domain)
			>>| (withSymbols
				\symbols. deserializeFromBase64 taskF symbols)

removeDomainTask :: Domain DistributedTaskId -> Task ()
removeDomainTask domain dTaskId = upd (appTasks ('Data.Map'.del dTaskId)) (domainTasks domain) @! ()
