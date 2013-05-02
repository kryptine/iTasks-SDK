implementation module iTasks.Framework.TaskState

import JSON
import iTasks.Framework.UIDefinition

from iTasks					import JSONEncode, JSONDecode
from iTasks.Framework.Task	import :: Event, :: TaskTime, :: TaskResult(..), :: TaskInfo(..), :: TaskRep(..), :: TaskServiceRep, :: TaskPart, :: TaskCompositionType
import iTasks.API.Core.SystemTypes

derive JSONEncode TIMeta, TIReduct, TaskTree, TaskListEntry, TaskListEntryState, TaskResult, TaskRep, TaskInfo
derive JSONDecode TIMeta, TIReduct, TaskTree, TaskListEntry, TaskListEntryState, TaskResult, TaskRep, TaskInfo

//IS ALSO DERIVED IN TASK STORE: SEEMS REDUNDANT
derive JSONEncode UIDef, UIAction, UIViewport, UIWindow, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONEncode UIControlSequence, UIActionSet, UIControlGroup, UIAbstractContainer
derive JSONEncode UIProgressOpts, UISliderOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UICodeOpts, UIGridOpts, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITaskletPHOpts, UITreeNode
derive JSONEncode UIMenuButtonOpts, UIButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts
derive JSONEncode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem

derive JSONDecode TaskCompositionType
derive JSONDecode UIDef, UIAction, UIViewport, UIWindow, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
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
