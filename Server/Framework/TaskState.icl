implementation module TaskState

import SystemTypes
from iTasks		import JSONEncode, JSONDecode
from Task		import :: Event, :: EditEvent, :: TaskTime
from GenUpdate	import :: UpdateMask
import JSON

derive JSONEncode TopInstance, TaskTree, ParallelMeta, ParallelItem, UpdateMask
derive JSONDecode TopInstance, TaskTree, ParallelMeta, ParallelItem, UpdateMask

instanceToTaskListItem :: !TopInstance -> TaskListItem
instanceToTaskListItem {TopInstance|instanceId,progress,management,tree,attributes}
	= {taskId = TaskId instanceId 0, taskMeta = attributes, progressMeta = Just progress, managementMeta = Just management, subItems = subItems tree}
where
	subItems (Left state)			= stateToTaskListItems state
	subItems _						= []

stateToTaskListItems :: !TaskTree -> [TaskListItem]
stateToTaskListItems (TCStep _ (Left context))			= stateToTaskListItems context
stateToTaskListItems (TCStep _ (Right (_,_,context)))	= stateToTaskListItems context
stateToTaskListItems (TCParallel _ _ subs)				= parallelToTaskListItems subs
stateToTaskListItems _									= []

parallelToTaskListItems :: ![ParallelItem]-> [TaskListItem]
parallelToTaskListItems [] = []
parallelToTaskListItems [{ParallelItem|taskId,progress,management,state,lastAttributes}:subs]
	= [{taskId = taskId, taskMeta = lastAttributes, progressMeta = progress, managementMeta = management, subItems = stateToTaskListItems state}
	  :parallelToTaskListItems subs]


