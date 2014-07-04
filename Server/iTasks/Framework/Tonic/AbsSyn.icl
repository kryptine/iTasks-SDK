implementation module iTasks.Framework.Tonic.AbsSyn

import Data.Graph
import Text.JSON
from GenEq import generic gEq

derive JSONEncode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, StepElem, StepCont, StepFilter, GParType,
  NodeContents

derive JSONDecode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, StepElem, StepCont, StepFilter, GParType,
  NodeContents

derive gEq
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, StepElem, StepCont, StepFilter, GParType,
  NodeContents

mkGNode :: GNodeType -> GNode
mkGNode nt = {GNode|nodeType=nt}
