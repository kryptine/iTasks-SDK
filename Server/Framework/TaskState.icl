implementation module TaskState

import SystemTypes, UIDefinition
from iTasks		import JSONEncode, JSONDecode
from Task		import :: Event, :: TaskTime, :: TaskResult(..), :: TaskInfo(..), :: TaskRep(..), :: TaskServiceRep, :: TaskPart, :: TaskCompositionType
import JSON_NG

derive JSONEncode TIMeta, TIReduct, TIResult, TaskTree, TaskListEntry, TaskListEntryState
derive JSONDecode TIMeta, TIReduct, TIResult, TaskTree, TaskListEntry, TaskListEntryState

//IS ALSO DERIVED IN TASK STORE: SEEMS REDUNDANT
derive JSONEncode UIDef, UIAction, UIViewport, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONEncode UIControlSequence, UIActionSet, UIControlGroup, UIAbstractContainer
derive JSONEncode UIProgressOpts, UISliderOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UICodeOpts, UIGridOpts, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITaskletPHOpts, UITreeNode
derive JSONEncode UIMenuButtonOpts, UIButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts
derive JSONEncode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem

derive JSONDecode TaskRep, TaskCompositionType
derive JSONDecode UIDef, UIAction, UIViewport, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONDecode UIControlSequence, UIActionSet, UIControlGroup, UIAbstractContainer
derive JSONDecode UIProgressOpts, UISliderOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UICodeOpts, UIGridOpts, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITaskletPHOpts, UITreeNode
derive JSONDecode UIMenuButtonOpts, UIButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts
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
