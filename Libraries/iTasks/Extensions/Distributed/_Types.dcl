definition module iTasks.Extensions.Distributed._Types

import iTasks

:: Remote_Task = E. a: Remote_Task (Task a) TaskAttributes Int & iTask a | Remote_Taks_NotUsed

:: Remote_Share = E. a w: Remote_Share (ReadWriteShared a w) & iTask a & iTask w | Remote_Share_NotUsed

:: Remote_TaskValue = E. a: Remote_TaskValue (TaskValue a) & iTask a | Remote_TaskValue_NotUsed
