definition module iTasks.Framework.Tonic

from iTasks.Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld
from iTasks.Framework.Engine import :: PublishedTask
import iTasks.Framework.Generic
from iTasks.API.Core.TaskCombinators import class tune
from iTasks.API.Core.Types import :: User
from iTasks.API.Core.Tasks import :: Task, :: InstanceNo
from iTasks.Framework.Tonic.AbsSyn import :: GinGraph, :: Graph, :: GEdge, :: GNode
from System.Time import :: Timestamp
from Data.Map import :: Map

:: TonicTune =
	{ tu_moduleName :: String
	, tu_taskName   :: String
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

derive class iTask TonicTrace, TraceType, TonicTune

:: UserTraceMap :== Map User (Map InstanceNo [TonicTrace])

:: UserGraphMap :== Map User GinGraph

tonicTraces :: Shared UserTraceMap

tonicLogin :: String -> Task Void

tonicPubTask :: String -> PublishedTask

tonicWrapTask :: String String [(String, Task ())] (Task a) -> Task a

tonicTune :: String String Int (Task a) -> Task a

instance tune TonicTune
