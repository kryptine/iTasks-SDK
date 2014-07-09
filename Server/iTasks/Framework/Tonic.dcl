definition module iTasks.Framework.Tonic

from iTasks.Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld
from iTasks.Framework.Engine import :: PublishedTask
import iTasks.Framework.Generic
from iTasks.API.Core.TaskCombinators import class tune
from iTasks.API.Core.Types import :: User
from iTasks.API.Core.Tasks import :: Task, :: InstanceNo
import iTasks.Framework.Tonic.AbsSyn
from System.Time import :: Timestamp
from Data.Map import :: Map

derive gEditor
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive gEditMeta
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive gDefault
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive gUpdate
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive gVerify
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive gText
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

:: TaskDict a = TaskDict & iTask a

:: VarName :== String

:: TonicRTMap :== Map TaskId TonicRT

tonicSharedRT :: Shared TonicRTMap

:: TonicRT =
  { trt_taskId       :: TaskId
  , trt_params       :: [(VarName, Task ())]
  , trt_bpref        :: (ModuleName, TaskName)
  , trt_bpinstance   :: Maybe TonicTask
  , trt_activeNodeId :: Maybe Int
  , trt_parentTaskId :: TaskId
  , trt_output       :: Maybe (Task ())
  }

derive class iTask TonicRT

tonicLogin :: String -> Task ()

tonicPubTask :: String -> PublishedTask

tonicWrapTaskBody :: ModuleName TaskName [(VarName, Task ())] (TaskDict a) (Task a) -> Task a // | iTask a

tonicWrapApp :: ModuleName TaskName Int (Task a) -> Task a

