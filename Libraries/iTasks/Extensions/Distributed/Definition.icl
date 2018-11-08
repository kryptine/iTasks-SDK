implementation module iTasks.Extensions.Distributed.Definition

import iTasks

derive class iTask DomainTaskReference, DomainDevice, DomainTaskState
derive JSONEncode TaskResult, TaskEvalInfo
derive JSONDecode TaskResult, TaskEvalInfo

appTasks :: (DTaskMap -> DTaskMap) DomainTaskState -> DomainTaskState
appTasks f s=:{tasks} = {s & tasks = f tasks}