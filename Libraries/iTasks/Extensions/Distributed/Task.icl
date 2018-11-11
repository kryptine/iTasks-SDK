implementation module iTasks.Extensions.Distributed.Task

import iTasks.Extensions.Distributed.Definition
import iTasks.Extensions.Distributed.SDS
import iTasks.Internal.Distributed.Formatter
import iTasks.Internal.Distributed.Symbols

import qualified Data.Map
import Data.Func

appendDomainTask :: Domain TaskAttributes (Task a) -> Task DistributedTaskId | iTask a
appendDomainTask domain attributes task
= upd appendT (domainTasks domain)
		>>= \{nextTaskId}. (return (nextTaskId-1))
where
	appendT {nextTaskId, tasks} = {nextTaskId = nextTaskId + 1
		, tasks = 'Data.Map'.put nextTaskId (DomainTask attributes serializedTask, NoClaim, serializeToBase64 NoValue) tasks}

	serializedTask = serializeToBase64 task

queueDomainTask :: Domain TaskAttributes (Task a) -> Task a | iTask a
queueDomainTask domain attr task = appendDomainTask domain attr task
	>>= \dTaskId. withSymbols
		\symbols. watch (domainTaskValue domain dTaskId symbols)
			>>* [OnValue ifDoubleStable]
where
	ifDoubleStable (Value (Value v True) _) = Just v
	ifDoubleStable _ = Nothing

claimDomainTask :: Domain DistributedTaskId Bool -> Task Bool
claimDomainTask domain dTaskId force = (get currentUser -&&- get currentDateTime)
	>>= \(user, time). get (domainTask domain dTaskId)
 	>>= \mbTask. case mbTask of
 		Nothing = throw ("no task with given identifier: " +++ toString dTaskId)
 		Just (DomainTask attributes taskF, claimStatus, result)
 		= case claimStatus of
 			NoClaim = upd (appTasks $ 'Data.Map'.alter (applyClaim user time) dTaskId) (domainTasks domain)
 				>>= \_. return ()
 			Claimed claimer
 			| not force = viewInformation "Claim failed" [] "Claiming the task has failed. Another user has already claimed this task."
 				>>= \_. return ()
 			= upd (appTasks $ 'Data.Map'.alter (applyClaim user time) dTaskId) (domainTasks domain)
 				>>= \_. return ()
where
	applyClaim user time (Just (dt, _, taskResult)) = Just (dt, Claimed (User user time), taskResult)

executeDomainTask :: Domain DistributedTaskId -> Task Bool
executeDomainTask domain dTaskId = get currentUser
	>>= \currentUser. get (domainTask domain dTaskId)
	>>= \mbTask. case mbTask of
		Nothing = throw ("no task with given identifier: " +++ toString dTaskId)
		Just (DomainTask attributes taskF, claimStatus, result)
		= case claimStatus of
			NoClaim = viewInformation "No claim" [] "Claim the task first." @! ()
			Claimed claimee = case claimee of
				User user time
				| currentUser == user = executeTask taskF
				= viewInformation "No claim" [] "Task is claimed by another user. Please claim the task first." @! ()
where
	executeTask taskF = withSymbols \symbols. deserializeFromBase64 taskF symbols

removeDomainTask :: Domain DistributedTaskId -> Task ()
removeDomainTask domain dTaskId = upd (appTasks ('Data.Map'.del dTaskId)) (domainTasks domain) @! ()
