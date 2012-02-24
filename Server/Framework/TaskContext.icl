implementation module TaskContext

import SystemTypes
from iTasks		import JSONEncode, JSONDecode
from Task		import :: Event, :: EditEvent, :: TaskTime
from GenUpdate	import :: UpdateMask
import JSON

derive JSONEncode TopInstance, TaskState, ParallelMeta, ParallelItem, UpdateMask
derive JSONDecode TopInstance, TaskState, ParallelMeta, ParallelItem, UpdateMask

instanceToTaskListItem :: !TopInstance -> TaskListItem
instanceToTaskListItem {TopInstance|instanceId,progress,management,state,attributes}
	= {taskId = taskId instanceId, taskMeta = attributes, progressMeta = Just progress, managementMeta = Just management, subItems = subItems state}
where
	taskId (Left session)	= TaskId 0 0
	taskId (Right topNo)	= TaskId topNo 0
	
	subItems (Left state)			= stateToTaskListItems state
	subItems _						= []

stateToTaskListItems :: !TaskState -> [TaskListItem]
stateToTaskListItems (TCStep _ (Left context))			= stateToTaskListItems context
stateToTaskListItems (TCStep _ (Right (_,_,context)))	= stateToTaskListItems context
stateToTaskListItems (TCParallel _ _ subs)				= parallelToTaskListItems subs
stateToTaskListItems _									= []

parallelToTaskListItems :: ![ParallelItem]-> [TaskListItem]
parallelToTaskListItems [] = []
parallelToTaskListItems [{ParallelItem|taskId,progress,management,state,lastAttributes}:subs]
	= [{taskId = taskId, taskMeta = lastAttributes, progressMeta = progress, managementMeta = management, subItems = stateToTaskListItems state}
	  :parallelToTaskListItems subs]


