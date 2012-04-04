implementation module TaskState

import SystemTypes, TUIDefinition
from iTasks		import JSONEncode, JSONDecode
from Task		import :: Event, :: EditEvent, :: TaskTime, :: TaskResult(..), :: TaskRep(..), :: TaskTUIRep, :: TaskServiceRep, :: TaskPart, :: TaskCompositionType
from GenUpdate	import :: UpdateMask
import JSON

derive JSONEncode TaskInstance, TaskTree, TaskListEntry, TaskListEntryState, TaskResult, TaskRep, TaskCompositionType, UpdateMask
derive JSONDecode TaskInstance, TaskTree, TaskListEntry, TaskListEntryState, TaskResult, TaskRep, TaskCompositionType, UpdateMask

//IS ALSO DERIVED IN TASK STORE: SEEMS REDUNDANT
derive JSONEncode TUIDef, TUIDefContent, TUIIcon, TUIHtml, TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONEncode TUIControlType
derive JSONEncode TUIButtonControl, TUIListItem
derive JSONEncode TUIContainer, TUIPanel, TUIWindow, TUITabContainer, TUITabItem, TUIBorderContainer, TUIBorderItem, TUIListContainer, TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUIRadioChoice, TUICheckChoice, TUISize, TUIVAlign, TUIHAlign, TUIDirection, TUIMinSize, TUIMargins

derive JSONDecode TUIDef, TUIDefContent, TUIIcon, TUIHtml, TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONDecode TUIControlType
derive JSONDecode TUIButtonControl, TUIListItem
derive JSONDecode TUIContainer, TUIPanel, TUIWindow, TUITabContainer, TUITabItem, TUIBorderContainer, TUIBorderItem, TUIListContainer, TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUIRadioChoice, TUICheckChoice, TUISize, TUIVAlign, TUIHAlign, TUIDirection, TUIMinSize, TUIMargins


