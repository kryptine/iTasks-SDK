implementation module iTasks.Extensions.Distributed.Task

import StdMisc, StdDebug

import qualified Data.Map
import iTasks.Extensions.Distributed.SDS

appendDomainTask :: (Task a) Domain -> Task TaskId | iTask a
appendDomainTask task domain
= get (sdsFocus (task, 'Data.Map'.newMap) (addDistributedTask domain))

appendDomainTaskForUser :: User (Task a) Domain -> Task TaskId | iTask a
appendDomainTaskForUser user task domain
= get (sdsFocus (task, workerAttributes user []) (addDistributedTask domain))

viewTaskResult :: TaskId Domain (Task a) -> Task a | iTask a
viewTaskResult (TaskId instanceId _) domain t
= watch (sdsFocus instanceId (domainTaskResult t domain))
	@? \v. case v of
		NoValue = trace_n "NoValue yet" NoValue
		Value v _ = trace_n ("Value" +++ toSingleLineText v) v

removeDomainTask :: TaskId Domain -> Task Bool
removeDomainTask taskId domain = undef