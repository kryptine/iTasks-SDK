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

viewTaskResult :: TaskId Domain (Task a) -> Task () | iTask a
viewTaskResult (TaskId instanceId _) domain t
= whileUnchanged (sdsFocus instanceId (domainTaskResult t domain))
	\v. case v of
		NoValue = viewInformation "No value yet" [] ()
		Value v _ = viewInformation "Got value" [] v @! ()

removeDomainTask :: TaskId Domain -> Task Bool
removeDomainTask taskId domain = undef