implementation module TaskContext

import SystemTypes
from iTasks		import JSONEncode, JSONDecode
from Task		import :: Event, :: EditEvent
from GenUpdate	import :: UpdateMask
import JSON

derive JSONEncode TaskContext, ProcessState, TaskState, ParallelMeta, ParallelItem, UpdateMask
derive JSONDecode TaskContext, ProcessState, TaskState, ParallelMeta, ParallelItem, UpdateMask

contextToTaskListItem :: !TaskContext -> TaskListItem
contextToTaskListItem (TaskContext topid _ pmeta mmeta tmeta scontext)
	= {taskId = taskId topid, taskMeta = tmeta, progressMeta = Just pmeta, managementMeta = Just mmeta, subItems = tsubprocs scontext}
where
	taskId (Left session)	= TaskId 0 0
	taskId (Right topNo)	= TaskId topNo 0
	
	tsubprocs (TTCRunning _ state)			= stateToTaskListItems state
	tsubprocs _								= []

stateToTaskListItems :: !TaskState -> [TaskListItem]
stateToTaskListItems (TCStep _ (Left context))			= stateToTaskListItems context
stateToTaskListItems (TCStep _ (Right (_,_,context)))	= stateToTaskListItems context
stateToTaskListItems (TCParallel _ _ _ subs)			= parallelToTaskListItems subs
stateToTaskListItems _									= []

parallelToTaskListItems :: ![ParallelItem]-> [TaskListItem]
parallelToTaskListItems [] = []
parallelToTaskListItems [{ParallelItem|taskId,progress,management,state,attributes}:subs]
	= [{taskId = taskId, taskMeta = attributes, progressMeta = progress, managementMeta = management, subItems = stateToTaskListItems state}
	  :parallelToTaskListItems subs]


