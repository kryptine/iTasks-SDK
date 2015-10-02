implementation module iTasks._Framework.TaskState

import Text.JSON
import iTasks.UI.Definition

from iTasks					import JSONEncode, JSONDecode
from iTasks._Framework.Task	import :: Event, :: TaskTime, :: TaskResult(..), :: TaskException(..), :: TaskEvalInfo(..), :: TaskRep(..), exception, :: TonicOpts(..)
                                 , :: TaskUIs(..), :: TaskLayout(..), :: TaskUILayout(..), :: TaskUITree(..), :: UITag
from iTasks._Framework.Tonic.AbsSyn import :: ExprId (..)
import Data.CircularStack
import iTasks.API.Core.Types
from   Graphics.Scalable.Internal import :: XAlign(..), :: YAlign(..), :: GridLayout(..), :: GridYLayout(..), :: GridXLayout(..), :: GridDimension(..), :: GridMajor(..)
import Data.Error
import Data.Either
from   StdList  import hd, map, dropWhile
from   StdTuple import snd
from   StdFunc  import o

derive JSONEncode TIMeta, TIValue, TIReduct, TaskTree, ParallelTaskState, ParallelTaskChange, TaskResult, TaskRep, TaskEvalInfo, TonicOpts, CircularStack
derive JSONDecode TIMeta, TIValue, TIReduct, TaskTree, ParallelTaskState, ParallelTaskChange, TaskResult, TaskRep, TaskEvalInfo, TonicOpts, CircularStack

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

taskIdFromTaskTree :: !TaskTree -> MaybeError TaskException TaskId
taskIdFromTaskTree (TCInit                  taskId _)         = Ok taskId
taskIdFromTaskTree (TCBasic                 taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCInteract              taskId _ _ _ _ _) = Ok taskId
taskIdFromTaskTree (TCInteractLocal         taskId _ _ _ _)   = Ok taskId
taskIdFromTaskTree (TCInteractViewOnly      taskId _ _ _ _)   = Ok taskId
taskIdFromTaskTree (TCInteractLocalViewOnly taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCInteract1             taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCInteract2             taskId _ _ _ _)   = Ok taskId
taskIdFromTaskTree (TCProject               taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCStep                  taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCParallel              taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCShared                taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCExposedShared         taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCStable                taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCDestroy               tt)               = taskIdFromTaskTree tt
taskIdFromTaskTree _                                          = Error (exception "Unable to obtain TaskId from TaskTree (TCNop or TCTasklet)")

taskTreeOfTaskId :: !TaskId !TaskTree -> Maybe TaskTree
taskTreeOfTaskId taskId tt
	= case taskIdFromTaskTree tt of
	      Error _    = Nothing
	      Ok taskId` = if (taskId == taskId`) (Just tt) (
	                      case tt of
                              TCStep _ _ (Left tree)        = taskTreeOfTaskId taskId tree
                              TCStep _ _ (Right (_,_,tree)) = taskTreeOfTaskId taskId tree
                              TCParallel _ _ trees          = case dropWhile isNothing (map (taskTreeOfTaskId taskId o snd) trees) of
                                                                 []     = Nothing
                                                                 mtrees = hd mtrees
                              TCProject  _ _ tree           = taskTreeOfTaskId taskId tree
                              TCShared   _ _ tree           = taskTreeOfTaskId taskId tree
                              TCExposedShared _ _ _ tree    = taskTreeOfTaskId taskId tree
                              TCDestroy  tree               = taskTreeOfTaskId taskId tree      // is this OK?
                              _                             = Nothing
                       )
