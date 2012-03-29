implementation module TaskState

import SystemTypes, TUIDefinition
from iTasks		import JSONEncode, JSONDecode
from Task		import :: Event, :: EditEvent, :: TaskTime, :: TaskResult(..), :: TaskRep(..), :: TaskTUIRep, :: TaskServiceRep, :: TaskPart, :: TaskCompositionType
from GenUpdate	import :: UpdateMask
import JSON

derive JSONEncode TaskInstance, TaskTree, TaskListEntry, TaskListEntryState, TaskResult, TaskRep, TaskCompositionType, ParallelMeta, ParallelItem, UpdateMask
derive JSONDecode TaskInstance, TaskTree, TaskListEntry, TaskListEntryState, TaskResult, TaskRep, TaskCompositionType, ParallelMeta, ParallelItem, UpdateMask

//IS ALSO DERIVED IN TASK STORE: SEEMS REDUNDANT
derive JSONEncode TUIDef, TUIDefContent, TUIIcon, TUIHtml, TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONEncode TUIControlType
derive JSONEncode TUIButtonControl, TUIListItem
derive JSONEncode TUIContainer, TUIPanel, TUIWindow, TUITabContainer, TUITabItem, TUIBorderContainer, TUIBorderItem, TUIListContainer, TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUIRadioChoice, TUICheckChoice, TUISize, TUIVAlign, TUIHAlign, TUIDirection, TUIMinSize, TUIMargins

derive JSONDecode TUIDef, TUIDefContent, TUIIcon, TUIHtml, TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONDecode TUIControlType
derive JSONDecode TUIButtonControl, TUIListItem
derive JSONDecode TUIContainer, TUIPanel, TUIWindow, TUITabContainer, TUITabItem, TUIBorderContainer, TUIBorderItem, TUIListContainer, TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUIRadioChoice, TUICheckChoice, TUISize, TUIVAlign, TUIHAlign, TUIDirection, TUIMinSize, TUIMargins

instanceToTaskListItem :: !TaskInstance -> TaskListItem
instanceToTaskListItem {TaskInstance|instanceNo,progress,management,result}
	= {taskId = TaskId instanceNo 0, taskMeta = attributes result, progressMeta = Just progress, managementMeta = Just management, subItems = subItems result}
where
	attributes (ValueResult _ _ (TaskRep (_,_,_,attr) _) _) = attr
	attributes _											= []
	
	subItems (ValueResult _ _ _ tree)	= stateToTaskListItems tree
	subItems _							= []

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


