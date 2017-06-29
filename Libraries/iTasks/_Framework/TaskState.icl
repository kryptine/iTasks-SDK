implementation module iTasks._Framework.TaskState

import Text.JSON
import iTasks.UI.Definition
import iTasks.WF.Definition

from iTasks._Framework.Task	import exception
from iTasks._Framework.TaskEval import :: TaskTime, :: TaskEvalInfo(..), :: TonicOpts(..)
from iTasks._Framework.Tonic.AbsSyn import :: ExprId (..)
import iTasks._Framework.Serialization
import Data.CircularStack
import iTasks.API.Core.Types
import Data.Error

derive JSONEncode TIMeta, TIValue, TIReduct, TaskTree, ParallelTaskState, ParallelTaskChange, TaskResult, TaskEvalInfo, TonicOpts, CircularStack
derive JSONDecode TIMeta, TIValue, TIReduct, TaskTree, ParallelTaskState, ParallelTaskChange, TaskResult, TaskEvalInfo, TonicOpts, CircularStack

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

taskIdFromTaskTree :: TaskTree -> MaybeError TaskException TaskId
taskIdFromTaskTree (TCInit                  taskId _)         = Ok taskId
taskIdFromTaskTree (TCBasic                 taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCInteract              taskId _ _ _ _)   = Ok taskId
taskIdFromTaskTree (TCProject               taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCStep                  taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCParallel              taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCShared                taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCExposedShared         taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCStable                taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCLayout                _ tt)             = taskIdFromTaskTree tt
taskIdFromTaskTree (TCDestroy               tt)               = taskIdFromTaskTree tt
taskIdFromTaskTree _                                          = Error (exception "Unable to obtain TaskId from TaskTree (TCNop or TCTasklet)")
