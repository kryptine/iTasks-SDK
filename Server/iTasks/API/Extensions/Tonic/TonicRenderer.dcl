definition module iTasks.API.Extensions.Tonic.TonicRenderer

from Data.Maybe import :: Maybe
import iTasks
from iTasks.Framework.Tonic import :: TonicTrace, :: UserTraceMap
from iTasks.Framework.Tonic.AbsSyn import :: GNode, :: GEdge
from iTasks.API.Extensions.Tonic.Toniclet import :: GraphletRenderer

:: TonicState =
  { traces     :: UserTraceMap
  , renderMode :: RenderMode
  }

:: RenderMode
  =  SingleUser User TaskId
  |  MultiUser [TaskId]

derive class iTask TonicState, RenderMode

tonicRenderer :: GraphletRenderer GNode GEdge

tracesForUserTaskId :: User TaskId UserTraceMap -> [TonicTrace]

activeUserTracesMap :: UserTraceMap [TaskId] -> Map User [TonicTrace]
