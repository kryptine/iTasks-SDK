implementation module iTasks.Framework.TaskState

import Text.JSON
import iTasks.Framework.UIDefinition

from iTasks					import JSONEncode, JSONDecode
from iTasks.Framework.Task	import :: Event, :: TaskTime, :: TaskResult(..), :: TaskInfo(..), :: TaskRep(..), :: TaskServiceRep, :: TaskPart, :: TaskCompositionType, :: EventNo
import iTasks.API.Core.SystemTypes

derive JSONEncode TIMeta, SessionInfo, TIReduct, TaskTree, TaskListEntry, TaskListEntryState, TaskResult, TaskRep, TaskInfo
derive JSONDecode TIMeta, SessionInfo, TIReduct, TaskTree, TaskListEntry, TaskListEntryState, TaskResult, TaskRep, TaskInfo

//IS ALSO DERIVED IN TASK STORE: SEEMS REDUNDANT
derive JSONEncode UIDef, UIAction, UIViewport, UIWindow, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONEncode UIControlStack, UISubUI, UISubUIStack
derive JSONEncode UIProgressOpts, UISliderOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UICodeOpts, UIGridOpts, UITreeOpts, UIIconOpts, UILabelOpts, UITreeNode
derive JSONEncode UIMenuButtonOpts, UIButtonOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts, UITabSetOpts, UITab, UITabOpts
derive JSONEncode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem, UIOryxOpts
derive JSONEncode UITaskletOpts, UITaskletPHOpts, UIEditletOpts

derive JSONDecode TaskCompositionType
derive JSONDecode UIDef, UIAction, UIViewport, UIWindow, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONDecode UIControlStack, UISubUI, UISubUIStack
derive JSONDecode UIProgressOpts, UISliderOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UICodeOpts, UIGridOpts, UITreeOpts, UIIconOpts, UILabelOpts, UITreeNode
derive JSONDecode UIMenuButtonOpts, UIButtonOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts, UITabSetOpts, UITab, UITabOpts
derive JSONDecode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem, UIOryxOpts
derive JSONDecode UITaskletOpts, UITaskletPHOpts, UIEditletOpts

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
