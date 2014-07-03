definition module iTasks.Framework.Tonic

from iTasks.Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld
from iTasks.Framework.Engine import :: PublishedTask
import iTasks.Framework.Generic
from iTasks.API.Core.TaskCombinators import class tune
from iTasks.API.Core.Types import :: User
from iTasks.API.Core.Tasks import :: Task, :: InstanceNo
from iTasks.Framework.Tonic.AbsSyn import :: GinGraph, :: Graph, :: GEdge, :: GNode, :: ModuleName, :: TaskName, :: TonicTask
from System.Time import :: Timestamp
from Data.Map import :: Map

:: TaskDict a = TaskDict & iTask a

:: VarName :== String

:: TonicRTMap :== Map TaskId TonicRT

tonicSharedRT :: Shared TonicRTMap

:: TonicRT =
  { trt_taskId       :: TaskId
  , trt_params       :: [(VarName, Task ())]
  , trt_bpref        :: (ModuleName, TaskName)
  , trt_bpinstance   :: Maybe TonicTask
  , trt_parentTaskId :: TaskId
  , trt_output       :: Maybe (Task ())
  }

:: TonicTune =
	{ tu_moduleName :: ModuleName
	, tu_taskName   :: TaskName
	, tu_nodeId     :: Int
	, tu_valAsStr   :: Maybe String
	, tu_isBind     :: Bool
	}

:: TraceType = EnterTrace | ExitTrace

:: TonicTrace =
	{ tr_traceType  :: !TraceType
	, tr_tuneInfo   :: !TonicTune
	, tr_traceUser  :: !User
	, tr_traceTime  :: !Timestamp
	}

derive class iTask TonicTrace, TraceType, TonicTune, TonicRT

:: UserTraceMap :== Map User (Map InstanceNo [TonicTrace])

:: UserGraphMap :== Map User GinGraph

tonicTraces :: Shared UserTraceMap

tonicLogin :: String -> Task Void

tonicPubTask :: String -> PublishedTask

tonicWrapTask :: ModuleName TaskName [(VarName, Task ())] (TaskDict a) (Task a) -> Task a // | iTask a

tonicTune :: ModuleName TaskName Int (Task a) -> Task a

instance tune TonicTune

