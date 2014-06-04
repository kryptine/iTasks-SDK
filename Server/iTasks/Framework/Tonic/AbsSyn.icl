implementation module iTasks.Framework.Tonic.AbsSyn

import Data.Graph
import Text.JSON
from GenEq import generic gEq

derive JSONEncode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, TonicInfo, StepElem, StepType, GParType

derive JSONDecode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, TonicInfo, StepElem, StepType, GParType

derive gEq
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, TonicInfo, StepElem, StepType, GParType

mkGNode :: GNodeType -> GNode
mkGNode nt = {GNode|nodeType=nt, nodeTonicInfo = Nothing}
