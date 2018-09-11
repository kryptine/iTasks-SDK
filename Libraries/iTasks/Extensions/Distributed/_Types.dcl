definition module iTasks.Extensions.Distributed._Types

from iTasks.WF.Definition import :: TaskAttributes, :: Task, class iTask, :: TaskValue
import iTasks.SDS.Definition

:: Remote_Task = E. a: Remote_Task (Task a) TaskAttributes Int & iTask a | Remote_Taks_NotUsed

:: Remote_Share = E. sds r w: Remote_Share (sds () r w) & RWShared sds & iTask r & iTask w | Remote_Share_NotUsed 

:: Remote_TaskValue = E. a: Remote_TaskValue (TaskValue a) & iTask a | Remote_TaskValue_NotUsed
