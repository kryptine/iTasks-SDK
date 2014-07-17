implementation module iTasks.Framework.TaskState

import Text.JSON
import iTasks.Framework.UIDefinition

from iTasks					import JSONEncode, JSONDecode
from iTasks.Framework.Task	import :: Event, :: TaskTime, :: TaskResult(..), :: TaskException(..), :: TaskEvalInfo(..), :: TaskRep(..), :: TaskServiceRep, :: TaskPart, :: EventNo
import iTasks.API.Core.Types

derive JSONEncode TIMeta, TIValue, TIReduct, TaskTree, TaskListEntry, TaskListEntryState, TaskResult, TaskRep, TaskEvalInfo
derive JSONDecode TIMeta, TIValue, TIReduct, TaskTree, TaskListEntry, TaskListEntryState, TaskResult, TaskRep, TaskEvalInfo

//IS ALSO DERIVED IN TASK STORE: SEEMS REDUNDANT
derive JSONEncode UIDef, UIContent, UIAction, UIViewport, UIWindow, UIControl, UIFSizeOpts, UISizeOpts, UIHSizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONEncode UIEmpty, UIForm, UIBlock
derive JSONEncode UIProgressOpts, UISliderOpts, UIGridOpts, UITreeOpts, UIIconOpts, UILabelOpts, UITreeNode
derive JSONEncode UIMenuButtonOpts, UIButtonOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts, UITabSetOpts, UITab, UITabOpts
derive JSONEncode UISize, UIBound, UIDirection, UIWindowType, UIHAlign, UIVAlign, UISideSizes, UIMenuItem
derive JSONEncode UITaskletOpts, UIEditletOpts, UIEmbeddingOpts

//derive JSONDecode TaskCompositionType
derive JSONDecode UIDef, UIContent, UIAction, UIViewport, UIWindow, UIControl, UIFSizeOpts, UISizeOpts, UIHSizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONDecode UIEmpty, UIForm, UIBlock
derive JSONDecode UIProgressOpts, UISliderOpts, UIGridOpts, UITreeOpts, UIIconOpts, UILabelOpts, UITreeNode
derive JSONDecode UIMenuButtonOpts, UIButtonOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts, UITabSetOpts, UITab, UITabOpts
derive JSONDecode UISize, UIBound, UIDirection, UIWindowType, UIHAlign, UIVAlign, UISideSizes, UIMenuItem
derive JSONDecode UITaskletOpts, UIEditletOpts, UIEmbeddingOpts

JSONEncode{|DeferredJSON|} _ (DeferredJSON a)
	= JSONEncode{|*|} False a
JSONEncode{|DeferredJSON|} _ (DeferredJSONNode json)
	= [json]

JSONDecode{|DeferredJSON|} _ []
	= (Just (DeferredJSONNode JSONNull), [])
JSONDecode{|DeferredJSON|} _ [x:xs]
	= ((Just (DeferredJSONNode x)), xs)
JSONDecode{|DeferredJSON|} _ l
	= (Nothing, l)

taskIdFromTaskTree :: TaskTree -> Maybe TaskId
taskIdFromTaskTree (TCInit taskId _) = Just taskId
taskIdFromTaskTree (TCBasic taskId _ _ _) = Just taskId
taskIdFromTaskTree (TCInteract taskId _ _ _ _ _) = Just taskId
taskIdFromTaskTree (TCInteractLocal taskId _ _ _ _) = Just taskId
taskIdFromTaskTree (TCInteractViewOnly taskId _ _ _ _) = Just taskId
taskIdFromTaskTree (TCInteractLocalViewOnly taskId _ _ _) = Just taskId
taskIdFromTaskTree (TCInteract1 taskId _ _ _) = Just taskId
taskIdFromTaskTree (TCInteract2 taskId _ _ _ _) = Just taskId
taskIdFromTaskTree (TCProject taskId _ _) = Just taskId
taskIdFromTaskTree (TCStep taskId _ _) = Just taskId
taskIdFromTaskTree (TCParallel taskId _) = Just taskId
taskIdFromTaskTree (TCShared taskId _ _) = Just taskId
taskIdFromTaskTree (TCExposedShared taskId _ _ _) = Just taskId
taskIdFromTaskTree (TCStable taskId _ _) = Just taskId
taskIdFromTaskTree _ = Nothing
