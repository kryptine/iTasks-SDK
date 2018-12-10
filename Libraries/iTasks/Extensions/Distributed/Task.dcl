definition module iTasks.Extensions.Distributed.Task

import iTasks

appendDomainTask :: (Task a)  Domain -> Task TaskId | iTask a

appendDomainTaskForUser :: User (Task a) Domain -> Task TaskId | iTask a

viewTaskResult :: TaskId Domain (Task a) -> Task () | iTask a

removeDomainTask :: TaskId Domain -> Task Bool
