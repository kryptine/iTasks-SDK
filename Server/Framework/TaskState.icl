implementation module TaskState

import SystemTypes, UIDefinition
from iTasks		import JSONEncode, JSONDecode
from Task		import :: Event, :: EditEvent, :: TaskTime, :: TaskResult(..), :: TaskRep(..), :: TaskServiceRep, :: TaskPart, :: TaskCompositionType
from GenUpdate	import :: UpdateMask
import JSON_NG

derive JSONEncode TIMeta, TIReduct, TIResult, TaskTree, TaskListEntry, TaskListEntryState, UpdateMask
derive JSONDecode TIMeta, TIReduct, TIResult, TaskTree, TaskListEntry, TaskListEntryState, UpdateMask

//IS ALSO DERIVED IN TASK STORE: SEEMS REDUNDANT
derive JSONEncode UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UILayoutOpts
derive JSONEncode UIProgressOpts, UISliderOpts, UIGridOpts, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITreeNode
derive JSONEncode UIMenuButtonOpts, UIActionButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts
derive JSONEncode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem

derive JSONDecode TaskRep, TaskCompositionType
derive JSONDecode UIDef, UIAction, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UILayoutOpts
derive JSONDecode UIProgressOpts, UISliderOpts, UIGridOpts, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITreeNode
derive JSONDecode UIMenuButtonOpts, UIActionButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts
derive JSONDecode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem

JSONEncode{|DeferredJSON|} (DeferredJSON a)
	= JSONEncode{|*|} a
JSONEncode{|DeferredJSON|} (DeferredJSONNode json)
	= [json]

JSONDecode{|DeferredJSON|} []
	= (Just (DeferredJSONNode JSONNull), [])
JSONDecode{|DeferredJSON|} [x:xs]
	= ((Just (DeferredJSONNode x)), xs)
JSONDecode{|DeferredJSON|} l
	= (Nothing, l)
