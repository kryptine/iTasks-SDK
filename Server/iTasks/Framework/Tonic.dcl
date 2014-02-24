definition module iTasks.Framework.Tonic

from iTasks.Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld
from iTasks.Framework.Engine import :: PublishedTask
import iTasks.Framework.Generic
from iTasks.API.Core.TaskCombinators import class tune
from iTasks.API.Core.Types import :: User
from iTasks.API.Core.Tasks import :: Task, :: InstanceNo
from iTasks.Framework.Tonic.AbsSyn import :: GinGraph, :: Graph, :: GEdge, :: GNode, :: TonicInfo
from System.Time import :: Timestamp
from Data.Map import :: Map

:: TraceType = EnterTrace | ExitTrace

:: TonicTrace =
  { traceType  :: !TraceType
  , tuneInfo   :: !TonicInfo
  , traceUser  :: !User
  , traceTime  :: !Timestamp
  }

// We need the predecessor and successor IDs to add edges to the new node. We
// need to explicitly store them, because once we remove the original node, we
// can't obtain these IDs from the graph structure anymore.
:: TonicReplace a =
  { trOrigNodeId :: !Int
  , trPredIds    :: ![Int]
  , trSuccIds    :: ![Int]
  , trModuleName :: !String
  , trTaskName   :: !String
  }

derive class iTask TonicTrace, TraceType

:: UserTraceMap :== Map User (Map TaskId [TonicTrace])

:: UserGraphMap :== Map User (Map TaskId GinGraph)

tonicTraces :: Shared UserTraceMap

tonicGraphs :: Shared UserGraphMap

tonicTune :: String String Int Int !(Task a) -> Task a

tonicBind :: String String Int Int !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b

tonicReflection :: String String !(Task a) -> Task a

tonicVarToSingleTask :: String String Int Int Int (Task a) -> Task a

tonicVarToListOfTask :: String String Int Int Int [Task a] -> [Task a]

instance tune TonicInfo

instance tune (TonicReplace a)

tonicLogin :: String -> Task Void

tonicPubTask :: String -> PublishedTask
