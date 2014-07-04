definition module iTasks.API.Extensions.Tonic.TonicRenderer

from Data.Maybe import :: Maybe
import iTasks
from iTasks.Framework.Tonic import :: TonicTrace, :: UserTraceMap
from iTasks.Framework.Tonic.AbsSyn import :: GNode, :: GEdge
from iTasks.API.Extensions.Tonic.Toniclet import :: TonicletRenderer

:: TonicState =
  { traces     :: UserTraceMap
  }

derive class iTask TonicState

tonicRenderer :: TonicletRenderer

tracesForUserInstance :: User InstanceNo UserTraceMap -> [TonicTrace]

activeUserTracesMap :: UserTraceMap [InstanceNo] -> Map User [TonicTrace]
